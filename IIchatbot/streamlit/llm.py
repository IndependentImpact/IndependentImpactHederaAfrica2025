from __future__ import annotations

import json
import re
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Optional

from openai import OpenAI, OpenAIError

from mcp_client import MCPClientError, MCPConnectionConfig, call_tool


class LLMError(RuntimeError):
    """Raised when an LLM provider returns an error."""


@dataclass(slots=True)
class LLMConfig:
    api_key: str
    model: str
    system_prompt: str | None = None
    temperature: float = 0.0
    max_output_tokens: int | None = 512

DEFAULT_SYSTEM_PROMPT = ("""
You are a data analyst with tool access. Use MCP tools to answer the user’s request even when the schema is unknown.

Tools
- get_data_model(): returns JSON-LD if the server has a schema cache.
- sparql_query({ "query": "<SPARQL>" }): executes SPARQL over the active dataset. The server may inject FROM <dataset>.

General rules
- Always attempt get_data_model() ONCE. If it’s empty/insufficient, DO NOT stop; proceed with autonomous graph exploration via SPARQL.
- Keep calling tools until you can answer confidently, or you have shown (with evidence) that the data isn’t present.
- Use fully qualified IRIs in SPARQL (<…>). Stick to SELECT, DISTINCT, FILTER, ORDER BY, LIMIT, GROUP BY.

Generic exploration recipe (follow in order)
1) Triple sanity check:
   SELECT (COUNT(?s) AS ?n) WHERE { ?s ?p ?o }

2) Discover structure:
   a) Predicates:
      SELECT DISTINCT ?p WHERE { ?s ?p ?o } ORDER BY ?p
   b) Types (if present):
      SELECT DISTINCT ?t WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?t } ORDER BY ?t
   c) Sample rows:
      SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 20

3) Build vocabulary map (optional):
   SELECT ?x ?label WHERE {
     ?x ?lbl ?label .
     VALUES ?lbl {
       <http://www.w3.org/2000/01/rdf-schema#label>
       <http://www.w3.org/2004/02/skos/core#prefLabel>
       <http://schema.org/name>
       <http://purl.org/dc/terms/title>
     }
   } LIMIT 100

4) Align user intent to graph:
   - If user mentions a concept/metric, propose 2–3 plausible graph patterns that could capture it.
   - Probe each hypothesis with a small COUNT or DISTINCT query.
   - Expand the best hypothesis with a detailed query (GROUP BY / ORDER BY) to produce the answer.

5) Stop only when:
   - You have a clear, supported answer (show brief evidence from the last query), OR
   - Multiple probes return no signal; then explain what was tried and why the data seems absent.

Answer format
- State the reasoning briefly.
- Present the result clearly (counts, tables, or bullet points).
"""
)

# Tool schemas (OpenAI “function calling” format)
OPENAI_TOOLS = [
    {
        "type": "function",
        "function": {
            "name": "get_data_model",
            "description": "Retrieve and cache the data model schema from Fluree datasets",
            "parameters": {"type": "object", "properties": {}},
        },
    },
    {
        "type": "function",
        "function": {
            "name": "sparql_query",
            "description": "Execute a SPARQL query against the session dataset",
            "parameters": {
                "type": "object",
                "properties": {
                    "query": {"type": "string", "description": "SPARQL query"},
                },
                "required": ["query"],
            },
        },
    },
]

def _mcp_content_text(payload: Dict[str, Any]) -> str:
    """Extract the text blocks from a tools/call result."""
    parts: List[str] = []
    for blk in payload.get("content") or []:
        if isinstance(blk, dict) and blk.get("type") == "text":
            parts.append(str(blk.get("text", "")))
        else:
            parts.append(json.dumps(blk, ensure_ascii=False))
    if payload.get("structuredContent") is not None:
        parts.append(json.dumps(payload["structuredContent"], ensure_ascii=False))
    return "\n".join(p for p in parts if p)

_URI_PATTERN = re.compile(r"<([^>]+)>|https?://[^\s>\"]+")


def _find_uris(text: str) -> List[str]:
    """Extract unique IRIs / URLs from an arbitrary text blob."""

    seen: Dict[str, None] = {}
    for match in _URI_PATTERN.finditer(text):
        uri = match.group(1) or match.group(0)
        if uri not in seen:
            seen[uri] = None
    return list(seen)


def _normalize_lines(text: str) -> List[str]:
    return [ln.strip() for ln in text.splitlines() if ln.strip()]

def _strip_code_fence(text: str) -> str:
    stripped = text.strip()
    if not stripped.startswith("```"):
        return stripped
    lines = stripped.splitlines()
    if len(lines) >= 2 and lines[0].startswith("```") and lines[-1].strip() == "```":
        return "\n".join(lines[1:-1]).strip()
    return stripped


def _json_has_signal(payload: Any) -> bool:
    if payload is None:
        return False
    if isinstance(payload, (bool, int, float)):
        return True
    if isinstance(payload, str):
        return bool(payload.strip())
    if isinstance(payload, list):
        return any(_json_has_signal(item) for item in payload)
    if isinstance(payload, dict):
        for key, value in payload.items():
            if key in {"@context", "context"}:
                continue
            if key in {"@graph", "graph"}:
                if isinstance(value, list):
                    return any(_json_has_signal(item) for item in value)
                return bool(value)
            if _json_has_signal(value):
                return True
        return False
    return False


def _schema_payload_seems_useful(text: str) -> bool:
    cleaned = text.strip()
    if not cleaned:
        return False
    if cleaned in {"{}", "[]"}:
        return False

    fence_stripped = _strip_code_fence(cleaned)

    try:
        parsed = json.loads(fence_stripped)
    except json.JSONDecodeError:
        normalized = re.sub(r"\s+", "", cleaned.lower())
        if "\"@graph\":[]" in normalized or "\"graph\":[]" in normalized:
            return False
        return True
    else:
        return _json_has_signal(parsed)

def _looks_like_global_triple_count(query: str) -> bool:
    normalized = re.sub(r"\s+", " ", query.strip().lower())
    return (
        normalized.startswith("select")
        and "count(" in normalized
        and "{ ?s ?p ?o }" in normalized.replace("\n", " ")
    )


def _maybe_run_structural_hint(
    *,
    query: str,
    insights: Optional[Dict[str, Any]],
    mcp_config: MCPConnectionConfig,
    on_tool_event: Optional[Callable[[Dict[str, str]], None]] = None,
) -> str:
    if not insights or not _looks_like_global_triple_count(query):
        return ""

    predicate_examples = insights.get("predicates") or []
    type_examples = insights.get("types") or []
    sample_triples = insights.get("sample_triples") or []
    subject_examples = insights.get("subjects") or []
    predicate_usage = insights.get("predicate_usage") or []
    type_usage = insights.get("type_usage") or []

    hints: List[str] = [
        "⚠️ The issued query counts every triple (?s ?p ?o).",
    ]

    ran_any = False

    for predicate in predicate_examples[:3]:
        alt_query = (
            "SELECT (COUNT(DISTINCT ?s) AS ?subjects)\n"
            "WHERE {\n"
            f"  ?s <{predicate}> ?o .\n"
            "}"
        )
        try:
            payload = call_tool(mcp_config, "sparql_query", {"query": alt_query})
            text = _mcp_content_text(payload) or "[no text content]"
        except MCPClientError as exc:
            text = f"[error executing heuristic query: {exc}]"

        hints.append(f"- Predicate {predicate}: {text.strip()[:400]}")
        ran_any = True

        if on_tool_event:
            on_tool_event(
                {
                    "name": "sparql_query (structure-hint)",
                    "request": json.dumps({"query": alt_query}, indent=2),
                    "response": text[:4000],
                }
            )

    for rdf_type in type_examples[:2]:
        alt_query = (
            "SELECT (COUNT(DISTINCT ?entity) AS ?entities)\n"
            "WHERE {\n"
            f"  ?entity <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{rdf_type}> .\n"
            "}"
        )
        try:
            payload = call_tool(mcp_config, "sparql_query", {"query": alt_query})
            text = _mcp_content_text(payload) or "[no text content]"
        except MCPClientError as exc:
            text = f"[error executing heuristic query: {exc}]"

        hints.append(f"- Entities of type {rdf_type}: {text.strip()[:400]}")
        ran_any = True

        if on_tool_event:
            on_tool_event(
                {
                    "name": "sparql_query (structure-hint)",
                    "request": json.dumps({"query": alt_query}, indent=2),
                    "response": text[:4000],
                }
            )

    if not ran_any:
        if predicate_examples:
            hints.append(
                "Consider counting specific predicates or grouping by rdf:type to focus on meaningful entities."
            )
        else:
            hints.append(
                "No structural insights were available yet. Run predicate/type discovery queries first."
            )

    if predicate_usage:
        hints.append(
            "Predicate usage snapshot:\n" + "\n".join(predicate_usage[:3])
        )

    if type_usage:
        hints.append(
            "Type usage snapshot:\n" + "\n".join(type_usage[:3])
        )

    if subject_examples:
        hints.append(
            "Sample subjects seen during exploration: "
            + ", ".join(subject_examples[:3])
            + ("…" if len(subject_examples) > 3 else "")
        )

    if sample_triples:
        hints.append(
            "Example triples:\n" + "\n".join(sample_triples[:3])
        )

    return "Auto-hint guidance:\n" + "\n".join(hints)

def _run_autonomous_exploration(
    mcp_config: MCPConnectionConfig,
    *,
    on_tool_event: Optional[Callable[[Dict[str, str]], None]] = None,
) -> tuple[str, Dict[str, Any]]:
    """Issue a small bundle of discovery queries when no schema is available."""

    discovery_steps = [
        (
            "Triple count",
            "SELECT (COUNT(?s) AS ?n) WHERE { ?s ?p ?o }",
        ),
        (
            "Predicates in use",
            "SELECT DISTINCT ?p WHERE { ?s ?p ?o } ORDER BY ?p",
        ),
        (
            "Predicate usage counts",
            """
            SELECT ?p (COUNT(*) AS ?triples)
            WHERE { ?s ?p ?o }
            GROUP BY ?p
            ORDER BY DESC(?triples)
            LIMIT 10
            """.strip(),
        ),
        (
            "Known rdf:type values",
            "SELECT DISTINCT ?t WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?t } ORDER BY ?t",
        ),
        (
            "Type usage counts",
            """
            SELECT ?t (COUNT(?s) AS ?entities)
            WHERE {
              ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?t
            }
            GROUP BY ?t
            ORDER BY DESC(?entities)
            LIMIT 10
            """.strip(),
        ),
        (
            "Sample triples",
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 20",
        ),
        (
            "Label hints",
            """
            SELECT ?x ?label WHERE {
              ?x ?lbl ?label .
              VALUES ?lbl {
                <http://www.w3.org/2000/01/rdf-schema#label>
                <http://www.w3.org/2004/02/skos/core#prefLabel>
                <http://schema.org/name>
                <http://purl.org/dc/terms/title>
              }
            } LIMIT 100
            """.strip(),
        ),
    ]

    exploration_chunks: list[str] = []
    insights: Dict[str, Any] = {
        "predicates": set(),
        "types": set(),
        "sample_triples": [],
        "subjects": [],
        "predicate_usage": [],
        "type_usage": [],
    }
    for title, query in discovery_steps:
        try:
            payload = call_tool(mcp_config, "sparql_query", {"query": query})
            text = _mcp_content_text(payload) or "[no text content]"
        except MCPClientError as exc:
            text = f"[error executing discovery query: {exc}]"

        exploration_chunks.append(f"{title}:\n{text}")

        if title == "Predicates in use":
            for uri in _find_uris(text):
                insights["predicates"].add(uri)
        elif title == "Known rdf:type values":
            for uri in _find_uris(text):
                insights["types"].add(uri)
        elif title == "Predicate usage counts":
            insights["predicate_usage"].extend(_normalize_lines(text))
        elif title == "Type usage counts":
            insights["type_usage"].extend(_normalize_lines(text))
        elif title == "Sample triples":
            normalized_lines = _normalize_lines(text)
            insights["sample_triples"].extend(normalized_lines)
            subject_candidates: List[str] = []
            for line in normalized_lines:
                uris = _find_uris(line)
                if uris:
                    subject_candidates.append(uris[0])
            if subject_candidates:
                insights["subjects"].extend(
                    uri for uri in subject_candidates if uri not in insights["subjects"]
                )
        if on_tool_event:
            on_tool_event(
                {
                    "name": "sparql_query (auto)",
                    "request": json.dumps({"query": query}, indent=2),
                    "response": text[:4000],
                }
            )

    predicate_list = sorted(insights["predicates"])
    type_list = sorted(insights["types"])
    insights["predicates"] = predicate_list
    insights["types"] = type_list
    insights["predicate_usage"] = insights["predicate_usage"][:10]
    insights["type_usage"] = insights["type_usage"][:10]
    insights["subjects"] = list(dict.fromkeys(insights["subjects"]))[:10]

    summary_lines: List[str] = []
    if predicate_list:
        display = ", ".join(predicate_list[:6])
        if len(predicate_list) > 6:
            display += ", …"
        summary_lines.append(
            f"Discovered {len(predicate_list)} distinct predicates. Examples: {display}"
        )
    if type_list:
        display = ", ".join(type_list[:6])
        if len(type_list) > 6:
            display += ", …"
        summary_lines.append(
            f"Discovered {len(type_list)} rdf:type values. Examples: {display}"
        )
    if insights["predicate_usage"]:
        summary_lines.append(
            "Predicate usage counts:\n" + "\n".join(insights["predicate_usage"][:5])
        )
    if insights["type_usage"]:
        summary_lines.append(
            "Type usage counts:\n" + "\n".join(insights["type_usage"][:5])
        )
    if insights["subjects"]:
        summary_lines.append(
            "Sample subjects:\n" + "\n".join(insights["subjects"][:3])
        )
    sample_preview = insights["sample_triples"][:3]
    if sample_preview:
        summary_lines.append("Sample triples:\n" + "\n".join(sample_preview))

    if summary_lines:
        exploration_chunks.append("Heuristic summary:\n" + "\n".join(summary_lines))

    return "\n\n".join(exploration_chunks), insights

def generate_answer_with_tools(
    config: LLMConfig,
    *,
    question: str,
    mcp_config: MCPConnectionConfig,
    optional_context_text: str = "",
    max_tool_loops: int = 6,
    on_tool_event: Optional[Callable[[Dict[str, str]], None]] = None,  # <- for UI trace
) -> str:
    client = OpenAI(api_key=config.api_key)

    system_prompt = config.system_prompt or DEFAULT_SYSTEM_PROMPT
    messages: list[dict] = [{"role": "system", "content": system_prompt}]
    if optional_context_text.strip():
        messages.append({"role": "system", "content": f"Context:\n{optional_context_text.strip()}"})
    messages.append({"role": "user", "content": question.strip()})

    # ------------------------------------------------------------------------------------
    # Preflight: ALWAYS call get_data_model() exactly once before entering the LLM loop.
    # If empty/insufficient, run autonomous exploration and attach its summary to the
    # tool result. We also emit on_tool_event for the preflight steps.
    # ------------------------------------------------------------------------------------
    auto_explored = False
    auto_insights: Dict[str, Any] | None = None
    preflight_tool_id = "preflight_get_data_model"

    # Add a synthetic assistant tool_call so the transcript looks standard
    messages.append({
        "role": "assistant",
        "content": None,
        "tool_calls": [{
            "id": preflight_tool_id,
            "type": "function",
            "function": {"name": "get_data_model", "arguments": "{}"},
        }],
    })

    try:
        payload = call_tool(mcp_config, "get_data_model", {})
        out_text = _mcp_content_text(payload) or "[no text content]"

        # 1) ✨ Log + append the get_data_model result FIRST
        if on_tool_event:
            on_tool_event({
                "name": "get_data_model",
                "request": "{}",
                "response": out_text[:4000],
            })

        messages.append({
            "role": "tool",
            "tool_call_id": preflight_tool_id,
            "name": "get_data_model",
            "content": out_text,
        })

        # 2) THEN decide whether to run exploration (and log those calls after)
        if not _schema_payload_seems_useful(out_text):
            auto_text, auto_insights = _run_autonomous_exploration(
                mcp_config, on_tool_event=on_tool_event
            )
            if auto_text:
                # Don’t back-edit the original tool message; just provide context for the LLM
                messages.append({
                    "role": "system",
                    "content": "Auto-exploration summary (schema was empty):\n" + auto_text
                })
            auto_explored = True
        else:
            auto_explored = True

    except MCPClientError as exc:
        fail_text = f"[error executing get_data_model: {exc}]"
        if on_tool_event:
            on_tool_event({
                "name": "get_data_model",
                "request": "{}",
                "response": fail_text[:4000],
            })
        messages.append({
            "role": "tool",
            "tool_call_id": preflight_tool_id,
            "name": "get_data_model",
            "content": fail_text,
        })
        # We intentionally do NOT explore here; the loop can still proceed.

    # ------------------------------------------------------------------------------------
    # Normal LLM-driven tool loop (unchanged), with structural hints wired in.
    # ------------------------------------------------------------------------------------
    try:
        for _ in range(max_tool_loops):
            resp = client.chat.completions.create(
                model=config.model,
                temperature=config.temperature,
                messages=messages,
                tools=OPENAI_TOOLS,
                tool_choice="auto",
            )
            msg = resp.choices[0].message

            # No tool calls => final answer
            if not getattr(msg, "tool_calls", None):
                return (msg.content or "").strip() if msg.content else "No content."

            # Append assistant message that contains tool_calls (REQUIRED)
            assistant_tool_calls = []
            for tc in msg.tool_calls:
                assistant_tool_calls.append({
                    "id": tc.id,
                    "type": "function",
                    "function": {
                        "name": tc.function.name,
                        "arguments": tc.function.arguments or "{}",
                    },
                })
            messages.append({
                "role": "assistant",
                "content": msg.content or None,
                "tool_calls": assistant_tool_calls,
            })

            # Execute each tool, then add a 'tool' message that replies to that tool_call_id
            for tc in msg.tool_calls:
                name = tc.function.name
                try:
                    args = json.loads(tc.function.arguments or "{}")
                except json.JSONDecodeError:
                    args = {}

                if name == "get_data_model":
                    # We already did a preflight call. Still, if the model asks again,
                    # we’ll comply (don’t break flow), but we won’t re-run exploration here.
                    payload = call_tool(mcp_config, "get_data_model", {})
                    out_text = _mcp_content_text(payload) or "[no text content]"
                elif name == "sparql_query":
                    q = args.get("query", "")
                    payload = call_tool(mcp_config, "sparql_query", {"query": q})
                    out_text = _mcp_content_text(payload) or "[no text content]"
                    hint_text = _maybe_run_structural_hint(
                        query=q,
                        insights=auto_insights,
                        mcp_config=mcp_config,
                        on_tool_event=on_tool_event,
                    )
                    if hint_text:
                        out_text = "\n\n".join(
                            part for part in [out_text.strip(), hint_text] if part
                        )
                else:
                    out_text = f"[Unknown tool: {name}]"

                if on_tool_event:
                    on_tool_event({
                        "name": name,
                        "request": json.dumps(args or {}, indent=2),
                        "response": out_text[:4000],
                    })

                messages.append({
                    "role": "tool",
                    "tool_call_id": tc.id,
                    "name": name,
                    "content": out_text,
                })

        return "I reached the tool-call limit without producing a final answer."
    except OpenAIError as exc:
        raise LLMError(str(exc)) from exc
