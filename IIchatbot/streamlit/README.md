# Fluree MCP Streamlit explorer

An interactive Streamlit dashboard that connects to a Fluree Model Context
Protocol (MCP) server and lets you:

- inspect the resources, tools, and prompts exposed by the Fluree MCP server;
- pull resource contents or tool results into a context builder; and
- ask natural language questions about your ledgers by forwarding the collected
  context to an OpenAI-compatible LLM.

The application reuses the public `mcp` Python client to speak SSE with the
server and the official `openai` SDK for LLM calls. The UI now matches the dark
landing page served by Nginx so the handoff from `/` to `/app` feels cohesive.

## Prerequisites

- Python 3.11+
- Access to a running Fluree MCP server
- An API key for an OpenAI-compatible LLM provider

## Installation

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Configuration

The Streamlit process expects the MCP base URL to be provided via the
`MCP_BASE_URL` environment variable. Docker Compose sets this automatically to
`http://mcp:8765/mcp`; when running locally you can export the variable yourself:

```bash
export MCP_BASE_URL="http://127.0.0.1:8765/mcp"
```

At runtime you still enter your LLM API key inside the sidebar. Keys are held in
Streamlit session state only and are never written to disk.

## Running the app

```bash
streamlit run app.py
```

Visit <http://localhost:8501> and click **Connect** in the sidebar. The app will
use the `MCP_BASE_URL` value, fetch the MCP capabilities, and unlock the tabs
for browsing resources, tools, prompts, and the Q&A workflow.

## Working with the UI

- **Overview tab** – displays the MCP `initialize` payload along with quick
  metrics showing how many resources, tools, and prompts are available.
- **Resources tab** – list and preview resources. Loading a resource trims its
  textual content to the configured character limit and adds it to the context
  builder.
- **Tools tab** – inspect server-side tools, call them with JSON arguments, and
  push the latest result into the context builder.
- **Prompts tab** – fetch prompt definitions from the MCP server and add them to
  the context builder.
- **LLM Q&A** – curate context blocks, add manual notes, and send the combined
  information with your question to the configured LLM.

The sidebar keeps the configuration minimal: connect/disconnect buttons, a live
connection status indicator, and LLM controls (model name, temperature, max
tokens, and system prompt).

## Security considerations

- API keys entered in the sidebar stay in Streamlit session state and are only
  used for outbound LLM calls.
- The app does not persist connection details, context blocks, or model
  responses to disk.
- Because the MCP URL is supplied by the host environment, verify the value of
  `MCP_BASE_URL` before launching the UI in production.

## Development notes

- `app.py` – Streamlit front-end, session management, and theming
- `mcp_client.py` – synchronous helper functions that wrap the asynchronous MCP
  client
- `llm.py` – wrapper around the OpenAI Responses API with tool orchestration

The helpers are intentionally small so you can adapt them as Fluree evolves the
MCP server.

