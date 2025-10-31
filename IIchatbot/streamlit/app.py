from __future__ import annotations

import html
import json
import os
from contextlib import nullcontext
from dataclasses import dataclass
from typing import Any, Dict, List
from pathlib import Path

import streamlit as st

from llm import DEFAULT_SYSTEM_PROMPT, LLMConfig, LLMError, generate_answer_with_tools
from mcp_client import (
    MCPClientError,
    MCPConnectionConfig,
    call_tool,
    extract_text_from_resource,
    fetch_connection_summary,
    get_prompt,
    read_resource,
    summarise_prompts,
    summarise_resources,
    summarise_tools,
)

# ---------------------------------------------------------------------------
# Environment variables
#      - make sure to set these in the .env file before strating the app
# ---------------------------------------------------------------------------

# Load .env from project root (only for local dev)
try:
    from dotenv import load_dotenv, find_dotenv
    dotenv_path = find_dotenv(usecwd=True)
    if not dotenv_path:
        root_env = Path(__file__).resolve().parents[1] / ".env"
        dotenv_path = str(root_env) if root_env.exists() else None

    if dotenv_path:
        load_dotenv(dotenv_path=dotenv_path, override=False)
except Exception:
    pass

DEFAULT_MCP = os.getenv("MCP_BASE_URL")
DEFAULT_LLM_API_KEY = os.getenv("LLM_API_KEY")

# Initial Streamlit page configuration
st.set_page_config(
    page_title="Fluree MCP LLM Explorer",
    page_icon="🗃️",
    layout="wide",
    initial_sidebar_state="collapsed",
)

# ---------------------------------------------------------------------------
# Styling
# ---------------------------------------------------------------------------

THEME_CSS = """
<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Plus+Jakarta+Sans:wght@600;700&display=swap');

:root {
    --brand-blue-500: #1E88E5;
    --brand-blue-700: #1976D2;
    --brand-blue-800: #1565C0;
    --trust-navy: #0A2240;
    --verify-green-700: #15803D;
    --accent-cyan: #06B6D4;
    --amber-500: #F59E0B;
    --amber-700: #B45309;
    --error-600: #DC2626;
    --bg: #FFFFFF;
    --surface: #F8FAFC;
    --border: #E2E8F0;
    --text: #0F172A;
    --text-2: #334155;
}

body, .stApp {
    background-color: var(--bg);
    color: var(--text);
    font-family: "Inter", system-ui, sans-serif;
}

.stApp h1,
.stApp h2,
.stApp h3,
.stApp h4,
.stApp h5,
.stApp h6 {
    font-family: "Plus Jakarta Sans", sans-serif;
    font-weight: 700;
    color: var(--brand-blue-800);
}

.stApp a,
.stMarkdown a {
    color: var(--brand-blue-700);
    font-weight: 600;
}

.stApp a:hover,
.stApp a:focus-visible {
    color: var(--brand-blue-500);
    text-decoration: underline;
}

.stApp .block-container {
    max-width: 1200px;
    padding: 2.5rem 3rem 4rem;
    margin-top: 3.5rem;
    margin-bottom: 4rem;
    background: var(--surface);
    border-radius: 28px;
    border: 1px solid var(--border);
    box-shadow: 0 32px 70px rgba(10, 34, 64, 0.08);
}

[data-testid="stSidebar"] {
    background: var(--surface);
    color: var(--text);
    border-right: 1px solid var(--border);
    padding: 1.5rem 1.25rem 2.5rem;
}

[data-testid="stSidebar"] * {
    color: var(--text) !important;
}

.profile-card {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    padding: 1.25rem 1.4rem;
    border-radius: 20px;
    background: linear-gradient(135deg, rgba(21, 101, 192, 0.16) 0%, rgba(6, 182, 212, 0.12) 100%);
    border: 1px solid rgba(21, 101, 192, 0.25);
    box-shadow: 0 18px 40px rgba(10, 34, 64, 0.12);
    margin-bottom: 1rem;
}

.profile-card .profile-initials {
    width: 52px;
    height: 52px;
    border-radius: 16px;
    background: linear-gradient(135deg, var(--brand-blue-700) 0%, var(--brand-blue-800) 100%);
    color: #ffffff;
    font-weight: 700;
    font-size: 1.1rem;
    display: flex;
    align-items: center;
    justify-content: center;
}

.profile-card h3 {
    margin: 0;
    font-size: 1.15rem;
    color: var(--brand-blue-800);
}

.profile-card p {
    margin: 0.2rem 0 0;
    font-size: 0.9rem;
    color: var(--text-2);
}

.hero-section {
    padding: 2.6rem 2.8rem;
    border-radius: 26px;
    background: linear-gradient(135deg, rgba(21, 101, 192, 0.14) 0%, rgba(30, 136, 229, 0.16) 55%, rgba(6, 182, 212, 0.18) 100%);
    border: 1px solid rgba(21, 101, 192, 0.2);
    box-shadow: 0 26px 70px rgba(10, 34, 64, 0.1);
    margin-bottom: 2.5rem;
    position: relative;
    overflow: hidden;
}

.hero-section::after {
    content: "";
    position: absolute;
    inset: 0;
    background: radial-gradient(circle at 20% 20%, rgba(21, 101, 192, 0.18), transparent 55%);
    opacity: 0.55;
    pointer-events: none;
}

.hero-section h1 {
    font-size: 2.7rem;
    margin-bottom: 1rem;
    max-width: 680px;
}

.hero-section p {
    max-width: 760px;
    font-size: 1.05rem;
    line-height: 1.65;
    color: var(--text-2);
    margin-bottom: 1.5rem;
}

.hero-badge {
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
    font-weight: 600;
    font-size: 0.85rem;
    color: var(--brand-blue-700);
    background: rgba(30, 136, 229, 0.12);
    border: 1px solid rgba(30, 136, 229, 0.24);
    border-radius: 999px;
    padding: 0.45rem 0.85rem;
    margin-bottom: 1rem;
}

.hero-meta {
    margin-top: 1.5rem;
    font-size: 0.9rem;
    color: var(--text-2);
}

.hero-actions {
    display: flex;
    gap: 0.85rem;
    flex-wrap: wrap;
}

.hero-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    text-decoration: none;
    border-radius: 999px;
    padding: 0.75rem 1.4rem;
    font-weight: 600;
    transition: all 0.2s ease;
}

.hero-btn.primary {
    background: linear-gradient(135deg, var(--brand-blue-700) 0%, var(--brand-blue-800) 100%);
    color: #ffffff;
    box-shadow: 0 16px 36px rgba(21, 101, 192, 0.25);
}

.hero-btn.primary:hover {
    transform: translateY(-2px);
    box-shadow: 0 20px 48px rgba(21, 101, 192, 0.3);
}

.hero-btn.secondary {
    background: transparent;
    border: 2px solid rgba(30, 136, 229, 0.4);
    color: var(--brand-blue-800);
}

.hero-btn.secondary:hover {
    background: rgba(30, 136, 229, 0.08);
}

.nav-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
    gap: 1.8rem;
    margin-bottom: 2.5rem;
}

.nav-card {
    position: relative;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    padding: 1.9rem 1.8rem;
    border-radius: 22px;
    background: var(--surface);
    border: 1px solid var(--border);
    box-shadow: 0 20px 45px rgba(10, 34, 64, 0.08);
    text-decoration: none;
    color: inherit;
    overflow: hidden;
}

.nav-card::after {
    content: "";
    position: absolute;
    inset: 0;
    background: linear-gradient(160deg, rgba(21, 101, 192, 0.12) 0%, rgba(6, 182, 212, 0) 65%);
    opacity: 0;
    transition: opacity 0.2s ease;
}

.nav-card:hover::after {
    opacity: 1;
}

.nav-card h3 {
    margin-bottom: 0.75rem;
    color: var(--brand-blue-800);
}

.nav-card p {
    margin-bottom: 1.25rem;
    color: var(--text-2);
}

.nav-card ul {
    margin: 0 0 1.25rem;
    padding-left: 1.15rem;
    color: var(--text-2);
}

.nav-card ul li {
    margin-bottom: 0.35rem;
}

.nav-card ul li::marker {
    color: var(--brand-blue-500);
}

.nav-card .nav-action {
    font-weight: 600;
    color: var(--brand-blue-700);
}

[data-testid="stSidebar"] .stButton>button {
    width: 100%;
    border-radius: 999px;
    font-weight: 600;
    padding: 0.6rem 0.9rem;
}

[data-testid="stSidebar"] .stButton>button[kind="primary"],
[data-testid="stSidebar"] .stButton>button:first-child {
    background: linear-gradient(135deg, var(--brand-blue-700) 0%, var(--brand-blue-800) 100%);
    color: #ffffff;
    box-shadow: 0 12px 30px rgba(21, 101, 192, 0.25);
    border: none;
}

[data-testid="stSidebar"] .stButton>button[kind="primary"]:hover,
[data-testid="stSidebar"] .stButton>button:first-child:hover {
    transform: translateY(-1px);
}

[data-testid="stSidebar"] .stButton>button[kind="secondary"],
[data-testid="stSidebar"] .stButton>button:nth-child(2) {
    background: transparent;
    border: 1px solid rgba(30, 136, 229, 0.35);
    color: var(--brand-blue-800);
}

.stTabs [role="tablist"] {
    border-bottom: 1px solid var(--border);
}

.stTabs [role="tab"] {
    background: transparent;
    color: var(--text-2);
    padding: 0.75rem 1.25rem;
    border-radius: 999px;
    margin-right: 0.6rem;
    border: none;
}

.stTabs [role="tab"][aria-selected="true"] {
    background: linear-gradient(135deg, var(--brand-blue-700) 0%, var(--brand-blue-800) 100%);
    color: #ffffff;
    font-weight: 600;
}

.stAlert {
    border-radius: 18px;
    border: 1px solid rgba(30, 136, 229, 0.28);
    background: rgba(248, 250, 252, 0.92);
}

.stButton>button {
    border-radius: 999px;
    font-weight: 600;
}

.stButton>button[kind="primary"] {
    background: linear-gradient(135deg, var(--brand-blue-700) 0%, var(--brand-blue-800) 100%);
    color: #ffffff;
    border: none;
}

.stButton>button[kind="secondary"] {
    background: transparent;
    border: 1px solid rgba(30, 136, 229, 0.35);
    color: var(--brand-blue-800);
}

.stTextInput>div>div>input,
.stTextArea textarea,
.stNumberInput>div>div>input,
.stSelectbox>div>div>select,
.stMultiSelect>div>div>div>div>div,
.stSlider>div>div>div {
    background: rgba(248, 250, 252, 0.9);
    color: var(--text);
    border-radius: 14px;
    border: 1px solid rgba(226, 232, 240, 0.9);
}

.stDataFrame, .stDataFrame [class*="row_heading"] {
    background: rgba(255, 255, 255, 0.9);
}

.stDataFrame [class*="col_heading"], .stDataFrame td {
    color: var(--text);
}

.stExpander {
    border: 1px solid var(--border);
    background: var(--surface);
    border-radius: 16px;
}

.stExpander summary {
    color: var(--brand-blue-800);
    font-weight: 600;
}

.profile-context-card {
    background: rgba(30, 136, 229, 0.08);
    border: 1px solid rgba(30, 136, 229, 0.2);
    border-radius: 20px;
    padding: 1.25rem 1.4rem;
    margin-bottom: 1.8rem;
}

.profile-context-card h4 {
    margin: 0 0 0.6rem;
    color: var(--brand-blue-800);
}

.profile-context-card p {
    margin: 0;
    white-space: pre-wrap;
    line-height: 1.55;
    color: var(--text-2);
}
</style>
"""

st.markdown(THEME_CSS, unsafe_allow_html=True)


# ---------------------------------------------------------------------------
# Session state helpers
# ---------------------------------------------------------------------------


@dataclass
class ContextBlock:
    identifier: str
    label: str
    content: str
    source: str


def ensure_session_defaults() -> None:
    st.session_state.setdefault("connection_config", None)
    st.session_state.setdefault("connection_summary", None)
    st.session_state.setdefault("connection_error", None)
    st.session_state.setdefault("resources_summary", [])
    st.session_state.setdefault("tools_summary", [])
    st.session_state.setdefault("prompts_summary", [])
    st.session_state.setdefault("resource_index", {})
    st.session_state.setdefault("tool_index", {})
    st.session_state.setdefault("context_blocks", [])
    st.session_state.setdefault("last_tool_result", None)
    st.session_state.setdefault("prompt_preview", None)
    st.session_state.setdefault("auto_connect_attempted", False)
    st.session_state.setdefault("profile_api_key", DEFAULT_LLM_API_KEY)
    st.session_state.setdefault("llm_model", "gpt-4o-mini")
    st.session_state.setdefault("llm_temperature", 0.1)
    st.session_state.setdefault("llm_max_tokens", 512)
    st.session_state.setdefault("system_prompt", DEFAULT_SYSTEM_PROMPT)


def add_context_block(block: ContextBlock) -> None:
    blocks: List[ContextBlock] = st.session_state.context_blocks
    for idx, existing in enumerate(blocks):
        if existing.identifier == block.identifier:
            blocks[idx] = block
            break
    else:
        blocks.append(block)


def remove_context_block(identifier: str) -> None:
    st.session_state.context_blocks = [
        block for block in st.session_state.context_blocks if block.identifier != identifier
    ]


ensure_session_defaults()


# ---------------------------------------------------------------------------
# Sidebar – profile, workspace, and connection settings
# ---------------------------------------------------------------------------

st.sidebar.header("Settings")
st.sidebar.markdown("---")
st.sidebar.subheader("LLM configuration")

llm_api_key = st.sidebar.text_input(
    "OpenAI compatible API key",
    type="password",
    key="profile_api_key",
    placeholder="sk-...",
    help="Provide an OpenAI-compatible API key. If `LLM_API_KEY` is set it will be used as the default.",
)
if DEFAULT_LLM_API_KEY:
    st.sidebar.caption("Prefilled from environment variable `LLM_API_KEY`.")

llm_model = st.sidebar.text_input("Model", key="llm_model")

temperature = st.sidebar.slider(
    "Temperature",
    min_value=0.0,
    max_value=1.0,
    step=0.05,
    key="llm_temperature",
)

max_tokens = st.sidebar.number_input(
    "Max output tokens",
    min_value=64,
    max_value=4096,
    step=32,
    key="llm_max_tokens",
)

system_prompt = st.sidebar.text_area(
    "System prompt",
    height=160,
    key="system_prompt",
    help="The system prompt will be included in each prompt made to the LLM.  Keep the default system prompt to ensure the AI agent works as expected.",
)

st.sidebar.markdown("---")
st.sidebar.subheader("MCP server connection")
st.sidebar.caption(f"Default MCP URL: `{DEFAULT_MCP or 'not set'}`")

if not DEFAULT_MCP:
    st.sidebar.error(
        "No MCP base URL configured. Set the MCP_BASE_URL environment variable which should point to the running Fluree MCP server."
    )


def handle_connect(show_spinner: bool = True, show_notification: bool = True) -> None:
    st.session_state.auto_connect_attempted = True

    if not DEFAULT_MCP:
        message = (
            "No MCP base URL configured. Set the MCP_BASE_URL environment variable "
            "which should point to the running Fluree MCP server."
        )
        st.session_state.connection_error = message
        if show_notification:
            st.sidebar.error(message)
        return

    config = MCPConnectionConfig(sse_url=DEFAULT_MCP)
    spinner = (
        st.spinner("Connecting to Fluree MCP server…") if show_spinner else nullcontext()
    )

    with spinner:
        try:
            summary = fetch_connection_summary(config)
        except MCPClientError as exc:
            st.session_state.connection_error = str(exc)
            if show_notification:
                st.sidebar.error(str(exc))
            return

    st.session_state.connection_config = config
    st.session_state.connection_summary = summary
    st.session_state.connection_error = None
    st.session_state.resources_summary = summarise_resources(summary.get("resources", []))
    st.session_state.tools_summary = summarise_tools(summary.get("tools", []))
    st.session_state.prompts_summary = summarise_prompts(summary.get("prompts", []))
    st.session_state.resource_index = {
        entry.get("uri"): entry for entry in summary.get("resources", []) if entry.get("uri")
    }
    st.session_state.tool_index = {
        entry.get("name"): entry for entry in summary.get("tools", []) if entry.get("name")
    }

    if show_notification:
        st.sidebar.success("Connected to the MCP server.")


def auto_connect_if_needed() -> None:
    if st.session_state.connection_config or st.session_state.auto_connect_attempted:
        return

    if not DEFAULT_MCP:
        st.session_state.connection_error = (
            "No MCP base URL configured. Set the MCP_BASE_URL environment variable "
            "which should point to the running Fluree MCP server."
        )
        st.session_state.auto_connect_attempted = True
        return

    handle_connect(show_spinner=False, show_notification=False)


auto_connect_if_needed()

if st.sidebar.button(
    "Reconnect",
    type="primary",
    disabled=not DEFAULT_MCP,
    width="stretch",
):
    handle_connect()

connection_status = "Connected" if st.session_state.connection_config else "Disconnected"
st.sidebar.metric("Connection status", connection_status)


# ---------------------------------------------------------------------------
# Main layout
# ---------------------------------------------------------------------------

connection_summary = st.session_state.connection_summary

query_params = st.query_params
raw_view = query_params.get("view", "home")
if isinstance(raw_view, list):
    active_view = raw_view[0] if raw_view else "home"
else:
    active_view = raw_view or "home"
if active_view not in {"home", "chatbot", "tools"}:
    active_view = "home"


def render_home_view() -> None:
    st.markdown(
        f"""
        <section class=\"hero-section\">
            <h1>Independent Impact Knowledge Explorer</h1>
            <p>Independent Impact balances openness with quality so participants can understand exactly what every token or verifiable credential represents. For more information visit
              <a href="https://independentimpact.org/">independentimpact.org</a>
            </p>
            <p>Explore your datasets with an AI-powerd impact browser by asking natural language questions.  <br>
            The assistant interfaces with the Fluree MCP server to generate queries and contextual explanations based on your prompts. <br>
            Alternatively explore individual MCP tools to query your data directly.</p>
            <div class=\"hero-actions\">
                <a class=\"hero-btn primary\" href=\"?view=chatbot\" target=\"_self\">Launch AI assistant</a>
                <a class=\"hero-btn secondary\" href=\"?view=tools\" target=\"_self\">Review MCP tools</a>
            </div>
        </section>
        <div class=\"nav-grid\">
            <a class=\"nav-card\" href=\"?view=chatbot\" target=\"_self\">
                <h3>Chatbot assistant</h3>
                <p>Ask questions to gain insights about your data.</p>
                <ul>
                    <li>Track every MCP tool call in the response trace.</li>
                    <li>Add prompts, resources and manual notes to the context builder.</li>
                </ul>
                <span class=\"nav-action\">Go to chatbot →</span>
            </a>
            <a class=\"nav-card\" href=\"?view=tools\" target=\"_self\">
                <h3>MCP tools</h3>
                <p>Inspect resources, tools and prompts exposed by the MCP server.</p>
                <ul>
                    <li>Preview metadata before adding it to the context builder.</li>
                    <li>Call MCP tools directly and reuse their outputs.</li>
                </ul>
                <span class=\"nav-action\">Go to MCP tools →</span>
            </a>
        </div>
        """,
        unsafe_allow_html=True,
    )

    if st.session_state.connection_error:
        st.error(f"Connection error: {st.session_state.connection_error}")
    elif connection_summary is None:
        st.info("Attempting to connect to the MCP server automatically…")


def render_tools_view() -> None:
    st.markdown(
        '<a class="back-link" href="?view=home" target="_self">← Back to main menu</a>',
        unsafe_allow_html=True,
    )

    if st.session_state.connection_error:
        st.error(f"Connection error: {st.session_state.connection_error}")
        return

    if not st.session_state.connection_config or connection_summary is None:
        st.info("Connecting to the MCP server…")
        return

    overview_tab, resources_tab, tools_tab, prompts_tab = st.tabs(
        ["Overview", "Resources", "Tools", "Prompts"]
    )

    with overview_tab:
        st.subheader("Server information")
        st.json(connection_summary.get("initialize", {}))

        st.markdown("---")
        st.subheader("Available capabilities")
        col1, col2, col3 = st.columns(3)
        col1.metric("Resources", len(st.session_state.resources_summary))
        col2.metric("Tools", len(st.session_state.tools_summary))
        col3.metric("Prompts", len(st.session_state.prompts_summary))

        if st.button("Refresh metadata", width="content"):
            handle_connect()

    with resources_tab:
        st.subheader("Discovered resources")
        resources_summary = st.session_state.resources_summary
        if not resources_summary:
            st.write("No resources were advertised by the server.")
        else:
            st.dataframe(resources_summary, width="stretch")

            resource_options = [entry["uri"] for entry in resources_summary if entry.get("uri")]
            selected_resources = st.multiselect(
                "Add these resources to the LLM context",
                resource_options,
                key="selected_resources",
            )
            max_chars = st.number_input(
                "Trim resource contents to (characters)",
                min_value=500,
                max_value=20000,
                value=4000,
                step=500,
            )

            if st.button("Load selected resources", disabled=not selected_resources):
                config: MCPConnectionConfig = st.session_state.connection_config
                for uri in selected_resources:
                    try:
                        result = read_resource(config, uri)
                        text = extract_text_from_resource(result)
                    except MCPClientError as exc:
                        st.error(f"Failed to read resource {uri}: {exc}")
                        continue

                    if not text:
                        st.warning(f"Resource {uri} did not return textual content.")
                        continue

                    if max_chars and len(text) > max_chars:
                        text = text[: max_chars - 1] + "…"

                    add_context_block(
                        ContextBlock(
                            identifier=f"resource::{uri}",
                            label=f"Resource: {uri}",
                            content=text,
                            source="resource",
                        )
                    )

                st.success("Resource contents added to the context builder.")

    with tools_tab:
        st.subheader("Available tools")
        tools_summary = st.session_state.tools_summary
        if not tools_summary:
            st.write("No tools are available or the server does not advertise any.")
        else:
            st.dataframe(tools_summary, width="stretch")

            tool_names = [entry["name"] for entry in tools_summary if entry.get("name")]
            selected_tool = st.selectbox("Tool", tool_names, key="selected_tool")
            tool_args_input = st.text_area("Tool arguments (JSON)", value="{}", key="tool_args")

            if st.button("Call tool", disabled=not selected_tool):
                try:
                    args = json.loads(tool_args_input) if tool_args_input.strip() else {}
                except json.JSONDecodeError as exc:
                    st.error(f"Invalid JSON arguments: {exc}")
                else:
                    config = st.session_state.connection_config
                    with st.spinner(f"Calling tool {selected_tool}..."):
                        try:
                            result = call_tool(config, selected_tool, arguments=args or None)
                        except MCPClientError as exc:
                            st.error(f"Tool call failed: {exc}")
                        else:
                            st.session_state.last_tool_result = {
                                "name": selected_tool,
                                "result": result,
                            }
                            st.success("Tool call completed.")
                            st.json(result)

            last_tool = st.session_state.last_tool_result
            if last_tool:
                def tool_result_to_text(payload: Dict[str, Any]) -> str:
                    parts: List[str] = []
                    content_blocks = payload.get("content") or []
                    for block in content_blocks:
                        if isinstance(block, dict) and block.get("type") == "text":
                            parts.append(str(block.get("text", "")))
                        else:
                            parts.append(json.dumps(block, indent=2, ensure_ascii=False))

                    structured = payload.get("structuredContent")
                    if structured is not None:
                        parts.append(json.dumps(structured, indent=2, ensure_ascii=False))
                    return "\n".join(part for part in parts if part)

                if st.button("Add last tool result to context"):
                    payload = last_tool["result"]
                    formatted = tool_result_to_text(payload)
                    if not formatted.strip():
                        st.warning("The last tool response did not include textual content.")
                    else:
                        add_context_block(
                            ContextBlock(
                                identifier=f"tool::{last_tool['name']}",
                                label=f"Tool result: {last_tool['name']}",
                                content=formatted,
                                source="tool",
                            )
                        )
                        st.success("Tool output added to the context builder.")

    with prompts_tab:
        st.subheader("Available prompts")
        prompts_summary = st.session_state.prompts_summary
        if not prompts_summary:
            st.write("No prompts were advertised by the server.")
        else:
            st.dataframe(prompts_summary, width="stretch")

            prompt_names = [entry["name"] for entry in prompts_summary if entry.get("name")]
            selected_prompt = st.selectbox("Prompt", prompt_names, key="selected_prompt")
            prompt_args_input = st.text_area(
                "Prompt arguments (JSON)", value="{}", key="prompt_args"
            )

            if st.button("Fetch prompt", disabled=not selected_prompt):
                try:
                    prompt_args = (
                        json.loads(prompt_args_input) if prompt_args_input.strip() else {}
                    )
                except json.JSONDecodeError as exc:
                    st.error(f"Invalid JSON arguments: {exc}")
                else:
                    config = st.session_state.connection_config
                    try:
                        prompt = get_prompt(
                            config, selected_prompt, arguments=prompt_args or None
                        )
                    except MCPClientError as exc:
                        st.error(f"Failed to fetch prompt: {exc}")
                    else:
                        st.session_state.prompt_preview = {
                            "name": selected_prompt,
                            "payload": prompt,
                        }
                        st.json(prompt)

            prompt_preview = st.session_state.prompt_preview
            if prompt_preview:
                if st.button("Add prompt content to context"):
                    payload = prompt_preview["payload"]
                    content = payload.get("messages") or payload.get("text")
                    formatted = json.dumps(payload, indent=2, ensure_ascii=False)
                    add_context_block(
                        ContextBlock(
                            identifier=f"prompt::{prompt_preview['name']}",
                            label=f"Prompt: {prompt_preview['name']}",
                            content=formatted,
                            source="prompt",
                        )
                    )
                    st.success("Prompt data added to the context builder.")


def render_chatbot_view(
    llm_api_key: str,
    llm_model: str,
    temperature: float,
    max_tokens: int,
    system_prompt: str,
) -> None:
    st.markdown(
        '<a class="back-link" href="?view=home" target="_self">← Back to main menu</a>',
        unsafe_allow_html=True,
    )

    if st.session_state.connection_error:
        st.error(f"Connection error: {st.session_state.connection_error}")
        return

    if not st.session_state.connection_config:
        st.info("Connecting to the MCP server…")
        return

    st.subheader("AI-powered Impact Browser")
    blocks: List[ContextBlock] = st.session_state.context_blocks
    if not blocks:
        st.write('<p>Ask questions about your impact data. <br> Optionally provide additional context about your dataset to be included in the prompt.</p>',
            unsafe_allow_html=True)
    else:
        for block in list(blocks):
            expander = st.expander(block.label, expanded=False)
            with expander:
                st.text_area(
                    "",
                    block.content,
                    height=200,
                    key=f"context_preview_{block.identifier}",
                )
                if st.button("Remove", key=f"remove_{block.identifier}"):
                    remove_context_block(block.identifier)

    manual_context = st.text_area(
        "Additional manual context (optional)", key="manual_context", height=150
    )

    question = st.text_area("Ask a question", key="user_question", height=120)

    if st.button("Generate answer", type="primary", key="btn_generate_answer"):
        if not question.strip():
            st.error("Please enter a question for the LLM.")
        elif not llm_api_key:
            st.error(
                "Please provide an API key in the profile sidebar or set the LLM_API_KEY environment variable."
            )
        elif not st.session_state.connection_config:
            st.error("Please connect to the MCP server first.")
        else:
            context_parts: List[str] = []
            context_parts.extend(
                b.content for b in st.session_state.context_blocks
            )
            if manual_context.strip():
                context_parts.append(manual_context.strip())
            context_text = "\n\n".join(
                part.strip() for part in context_parts if part.strip()
            )

            config = LLMConfig(
                api_key=llm_api_key,
                model=llm_model,
                system_prompt=system_prompt,
                temperature=float(temperature),
                max_output_tokens=int(max_tokens),
            )

            st.session_state.setdefault("call_trace", [])
            st.session_state["call_trace"].clear()

            with st.spinner("Thinking, querying and answering..."):
                try:
                    answer = generate_answer_with_tools(
                        config,
                        question=question,
                        mcp_config=st.session_state.connection_config,
                        optional_context_text=context_text,
                        on_tool_event=lambda ev: st.session_state["call_trace"].append(ev),
                    )
                except LLMError as exc:
                    st.error(f"Model call failed: {exc}")
                else:
                    st.markdown("### Answer")
                    st.write(answer)

            if st.session_state["call_trace"]:
                st.markdown("### Tool call trace")
                for i, ev in enumerate(st.session_state["call_trace"], 1):
                    with st.expander(f"Step {i}: {ev.get('name','(unknown)')}"):
                        st.code(ev.get("request", ""), language="json")
                        st.code(ev.get("response", ""), language="json")


if active_view == "home":
    render_home_view()
elif active_view == "tools":
    render_tools_view()
else:
    render_chatbot_view(llm_api_key, llm_model, temperature, max_tokens, system_prompt)
