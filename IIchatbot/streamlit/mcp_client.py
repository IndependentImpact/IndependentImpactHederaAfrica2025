"""Utility helpers for working with the Fluree MCP server.

This module centralises all interactions with the Model Context Protocol
client so that the Streamlit UI can call simple synchronous helper
functions.  The actual MCP client runs asynchronously, so the helpers in
this file wrap the asynchronous logic and expose a blocking API that is
easy to call from Streamlit callbacks.

The helpers intentionally keep the interface generic – the functions work
with any MCP server that exposes resources, prompts, or tools.  They are
therefore suitable for the Fluree MCP server as well as future MCP
servers the user may want to connect to.
"""

from __future__ import annotations

import asyncio
import json
import logging
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List

import httpx



logger = logging.getLogger(__name__)
# Enable DEBUG logs to the terminal Streamlit was launched from
if not logger.handlers:
    _h = logging.StreamHandler()
    _h.setFormatter(logging.Formatter("[%(levelname)s] %(asctime)s %(name)s: %(message)s"))
    logger.addHandler(_h)
logger.setLevel(logging.DEBUG)

# ---- debug helpers (mask secrets + pretty-print headers) ----
def _mask(v: str | None) -> str | None:
    if not v:
        return v
    if len(v) <= 8:
        return "****"
    return v[:4] + "..." + v[-4:]

def _dump_headers(tag: str, headers: dict | None) -> str:
    headers = headers or {}
    safe = {}
    for k, v in headers.items():
        if k.lower() in {"authorization", "api-key", "x-api-key"}:
            safe[k] = _mask(v)
        else:
            safe[k] = v
    return f"{tag} headers={safe}"


class MCPClientError(RuntimeError):
    """Base error raised when an MCP operation fails."""


class MCPConnectionError(MCPClientError):
    """Raised when the client cannot connect or initialise the server."""


class MCPToolError(MCPClientError):
    """Raised when a tool call fails."""


@dataclass(slots=True)
class MCPConnectionConfig:
    """Configuration needed to talk to an MCP server."""

    sse_url: str
    """The HTTP(S) endpoint that exposes the server-sent-events transport."""

    authorization_header: str | None = None
    """Optional value for the ``Authorization`` header."""

    extra_headers: Dict[str, str] = field(default_factory=dict)
    """Any additional HTTP headers that should be sent to the server."""

    http_timeout: float = 10.0
    """Timeout in seconds for HTTP requests to the MCP server."""

    sse_read_timeout: float = 300.0
    """Timeout in seconds while waiting for events on the SSE stream."""

    read_timeout: float | None = 60.0
    """
    Timeout for waiting on individual MCP responses.  ``None`` disables the
    timeout and lets the protocol determine how long to wait.
    """

    def build_headers(self) -> Dict[str, str]:
        """Return all HTTP headers that should be used for a request."""

        headers = dict(self.extra_headers)
        if self.authorization_header:
            headers["Authorization"] = self.authorization_header
        return headers


def _run_async(coro: Any) -> Any:
    """Run an async coroutine from synchronous code.

    Streamlit executes callbacks synchronously, so every asynchronous MCP
    call needs to be driven by an event loop created on demand.  The helper
    handles the common ``RuntimeError`` raised when ``asyncio.run`` is
    invoked from within an existing loop (which can happen during testing
    or when Streamlit upgrades to an async runtime).  In that case we spin
    up a fresh loop manually.
    """

    try:
        return asyncio.run(coro)
    except RuntimeError as exc:  # pragma: no cover - defensive branch
        if "event loop" in str(exc):
            loop = asyncio.new_event_loop()
            try:
                return loop.run_until_complete(coro)
            finally:
                loop.close()
        raise

async def _transport_initialize(config: MCPConnectionConfig) -> str:
    """
    POST /mcp with 'Accept: application/json, text/event-stream' and return the session id
    from the 'mcp-session-id' response header. Do NOT read the streaming body.
    """
    # Normalize URL
    url = config.sse_url.rstrip("/")
    if not url.endswith("/mcp"):
        url = url + "/mcp"

    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json, text/event-stream",  # server requires BOTH
    }
    headers.update(config.build_headers())

    body = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2025-06-18",
            "clientInfo": {"name": "streamlit-client", "version": "0.1"},
            "capabilities": {},
        },
    }

    timeout = httpx.Timeout(
        connect=config.http_timeout,
        read=2.0,                 # tiny read window; we’re not consuming SSE body
        write=config.http_timeout,
        pool=config.http_timeout,
    )

    logger.debug("INIT → POST %s | %s | body=%s",
                 url, _dump_headers("init", headers), json.dumps(body)[:300])

    try:
        async with httpx.AsyncClient(timeout=timeout) as client:
            # Use .stream so we don’t accidentally consume the SSE body
            async with client.stream("POST", url, headers=headers, json=body) as resp:
                ctype = resp.headers.get("content-type", "")
                logger.debug("INIT ← %s content-type=%s headers=%s",
                             resp.status_code, ctype, dict(resp.headers))

                # The server answers 200 + text/event-stream and sets mcp-session-id
                if resp.status_code != 200:
                    # Try to grab a small preview to help debugging
                    try:
                        preview = (await resp.aread())[:400]
                    except Exception:
                        preview = b""
                    raise MCPConnectionError(
                        f"Initialize failed: {resp.status_code} "
                        f"ctype={ctype!r} preview={preview.decode(errors='ignore')!r}"
                    )

                session_id = (
                    resp.headers.get("mcp-session-id")
                    or resp.headers.get("Mcp-Session-Id")
                )
                logger.debug("INIT session_id=%s", session_id)

                if not session_id:
                    # Some proxies strip headers; surface a clear error
                    raise MCPConnectionError(
                        "Initialize succeeded but no 'mcp-session-id' header was returned."
                    )

                return session_id

    except httpx.HTTPError as exc:
        # Surface precise transport failures (DNS, connect timeout, TLS, etc.)
        raise MCPConnectionError(f"HTTP error during initialize: {exc!r}") from exc

async def _rpc_post(
    base_url: str,
    session_id: str,
    method: str,
    params: dict | None,
    config: MCPConnectionConfig,
    req_id: int,
) -> dict:
    url = base_url.rstrip("/")
    if not url.endswith("/mcp"):
        url = url + "/mcp"

    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json, text/event-stream",
        "Mcp-Session-Id": session_id,
    }
    headers.update(config.build_headers())

    body = {"jsonrpc": "2.0", "id": req_id, "method": method, "params": params or {}}

    timeout = httpx.Timeout(
        connect=config.http_timeout,
        read=config.http_timeout,
        write=config.http_timeout,
        pool=config.http_timeout,
    )

    async with httpx.AsyncClient(timeout=timeout) as client:
        resp = await client.post(url, headers=headers, json=body)
        # Server sometimes replies as SSE, sometimes as JSON. Handle both.
        ctype = (resp.headers.get("content-type") or "").lower()

        if "application/json" in ctype:
            data = resp.json()
            if resp.status_code != 200 or "error" in data:
                raise MCPClientError(f"{method} failed: {resp.status_code} {data}")
            return data.get("result", {})

        # Fallback: treat entire body as one SSE 'message'
        raw = await resp.aread()
        try:
            # The typical SSE reply contains `event: message` + `data: {...}`
            # Many servers send only the JSON line; handle both.
            text = raw.decode(errors="ignore")
            for line in text.splitlines():
                if line.startswith("data:"):
                    obj = json.loads(line[5:].strip())
                    if "error" in obj:
                        raise MCPClientError(f"{method} failed: {obj}")
                    return obj.get("result", {})
            # If there was no 'data:' prefix, maybe the whole body is JSON
            obj = json.loads(text)
            if "error" in obj:
                raise MCPClientError(f"{method} failed: {obj}")
            return obj.get("result", {})
        except json.JSONDecodeError:
            preview = raw[:200]
            raise MCPClientError(f"{method} unexpected response (ctype={ctype}, status={resp.status_code}): {preview!r}")

async def _fetch_connection_summary(config: MCPConnectionConfig) -> dict[str, Any]:
    url = config.sse_url.rstrip("/")
    if not url.endswith("/mcp"):
        url = url + "/mcp"
    logger.debug("SUMMARY using endpoint %s", url)

    session_id = await _transport_initialize(config)
    logger.debug("SUMMARY: session_id=%s", session_id)

    async def collect(method: str, key: str, start_id: int) -> list[dict]:
        items: list[dict] = []
        cursor = None
        req_id = start_id
        while True:
            params = {"cursor": cursor} if cursor else {}
            result = await _rpc_post(config.sse_url, session_id, method, params, config, req_id)
            req_id += 1
            page_items = result.get(key) or result.get("items") or result.get("data") or []
            items.extend(page_items)
            cursor = result.get("nextCursor")
            if not cursor:
                return items

    tools     = await collect("tools/list",     "tools",     start_id=2)
    resources = await collect("resources/list", "resources", start_id=102)
    prompts   = await collect("prompts/list",   "prompts",   start_id=202)

    # Build a minimal initialize block for the UI
    init_result = {
        "protocolVersion": "2025-06-18",
        "serverInfo": {"name": "fluree-mcp-server", "version": "1.0.0"},
        "capabilities": {"tools": {}, "resources": {}, "prompts": {}},
        "instructions": None,
    }

    summary = {
        "initialize": init_result,
        "resources": resources,
        "tools": tools,
        "prompts": prompts,
    }
    return summary

def fetch_connection_summary(config: MCPConnectionConfig) -> dict[str, Any]:
    """Synchronously fetch connection information for display in the UI."""

    return _run_async(_fetch_connection_summary(config))


# ---------- RESOURCES (HTTP) ----------

async def _read_resource_http(
    config: MCPConnectionConfig,
    uri: str,
) -> dict[str, Any]:
    # 1) Obtain a session id via transport initialize
    session_id = await _transport_initialize(config)
    # 2) Do a JSON-RPC POST for resources/read
    result = await _rpc_post(
        base_url=config.sse_url,
        session_id=session_id,
        method="resources/read",
        params={"uri": uri},
        config=config,
        req_id=1,
    )
    return result

def read_resource(config: MCPConnectionConfig, uri: str) -> dict[str, Any]:
    """Synchronously read the contents of a resource (HTTP-only)."""
    return _run_async(_read_resource_http(config, uri))


# ---------- TOOLS (HTTP) ----------

async def _call_tool_http(
    config: MCPConnectionConfig,
    name: str,
    arguments: dict[str, Any] | None,
) -> dict[str, Any]:
    # 1) Obtain a session id
    session_id = await _transport_initialize(config)
    # 2) Call the tool
    result = await _rpc_post(
        base_url=config.sse_url,
        session_id=session_id,
        method="tools/call",
        params={"name": name, "arguments": arguments or {}},
        config=config,
        req_id=1,
    )
    return result

def call_tool(
    config: MCPConnectionConfig,
    name: str,
    arguments: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Synchronously execute a tool (HTTP-only)."""
    return _run_async(_call_tool_http(config, name, arguments))


# ---------- PROMPTS (HTTP) ----------

async def _get_prompt_http(
    config: MCPConnectionConfig,
    name: str,
    arguments: dict[str, str] | None = None,
) -> dict[str, Any]:
    # 1) Obtain a session id
    session_id = await _transport_initialize(config)
    # 2) Fetch the prompt definition
    result = await _rpc_post(
        base_url=config.sse_url,
        session_id=session_id,
        method="prompts/get",
        params={"name": name, "arguments": arguments or {}},
        config=config,
        req_id=1,
    )
    return result

def get_prompt(
    config: MCPConnectionConfig,
    name: str,
    arguments: dict[str, str] | None = None,
) -> dict[str, Any]:
    """Synchronously retrieve a prompt definition (HTTP-only)."""
    return _run_async(_get_prompt_http(config, name, arguments))


def extract_text_from_resource(result: dict[str, Any]) -> str:
    """Return the textual contents from a ``read_resource`` response."""

    contents = result.get("contents", [])
    text_parts: List[str] = []

    for item in contents:
        if not isinstance(item, dict):
            continue
        if "text" in item:
            text_parts.append(str(item["text"]))
        elif "blob" in item:
            # Blob values are base64 encoded.  Decoding large blobs could be
            # expensive, so we log and skip them rather than raising.
            logger.debug("Skipping binary blob contents for resource")

    return "\n".join(text_parts)


def summarise_resources(resources: Iterable[dict[str, Any]]) -> List[dict[str, Any]]:
    """Extract display friendly fields from a list of resources."""

    summary: List[dict[str, Any]] = []
    for resource in resources:
        summary.append(
            {
                "uri": resource.get("uri"),
                "description": resource.get("description"),
                "mimeType": resource.get("mimeType"),
                "size": resource.get("size"),
            }
        )
    return summary


def summarise_tools(tools: Iterable[dict[str, Any]]) -> List[dict[str, Any]]:
    """Extract the key properties for tools for easy display."""

    summary: List[dict[str, Any]] = []
    for tool in tools:
        summary.append(
            {
                "name": tool.get("name"),
                "description": tool.get("description"),
                "inputSchema": tool.get("inputSchema"),
                "outputSchema": tool.get("outputSchema"),
            }
        )
    return summary


def summarise_prompts(prompts: Iterable[dict[str, Any]]) -> List[dict[str, Any]]:
    """Extract key prompt metadata for display."""

    summary: List[dict[str, Any]] = []
    for prompt in prompts:
        summary.append(
            {
                "name": prompt.get("name"),
                "description": prompt.get("description"),
                "arguments": prompt.get("arguments"),
            }
        )
    return summary
