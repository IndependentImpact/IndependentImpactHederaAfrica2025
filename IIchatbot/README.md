# Streamlit Fluree MCP stack

This repository packages a dark-themed Streamlit client, a Fluree MCP server,
and an Nginx front-end into a single Docker Compose deployment. The aim is to
provide a cohesive landing experience on `/` and a streamlined workflow on
`/app` where analysts can explore Fluree datasets through the Model Context
Protocol (MCP).

## Repository layout

- `docker-compose.yml` – orchestrates the MCP server, Streamlit UI, and Nginx
- `mcp/` – Docker build context for the `fluree-mcp-server` image
- `streamlit/` – Streamlit application, requirements, and documentation
- `nginx/` – static landing page and production Nginx configuration

## Prerequisites

- Docker 24+ and Docker Compose plugin
- A Fluree instance accessible by the MCP server
- TLS certificates on the host (Nginx mounts `/etc/letsencrypt` read-only)

## Environment configuration

Create a `.env` file in the repository root before running Docker Compose. The
values below match the variables consumed in `docker-compose.yml` and the MCP
entrypoint:

```dotenv
# Fluree connection (required)
FLUREE_BASE_URL=https://datadudes2.xyz/fluree
DATASET=test/mcp

# Optional authentication (either environment variable or Docker secret)
FLUREE_API_KEY=replace-with-api-key

# Optional advanced configuration
# DATASETS=test/mcp,foo/bar
# DATASET_POLICIES=test/mcp=readOnly;foo/bar=maskPII
```

Additional notes:

- The Streamlit container reads `MCP_BASE_URL` from Compose and automatically
  connects to `http://mcp:8765/mcp` when you click **Connect** in the UI.
- If you prefer Docker secrets for the Fluree API key, create a file named
  `fluree_api_key` in the `secrets/` directory and reference it via
  `docker-compose.yml`.

## Running the stack

Build and start all services:

```bash
docker compose up --build -d
```

Useful follow-up commands:

- `docker compose logs -f mcp` – stream MCP server logs
- `docker compose logs -f streamlit` – inspect UI logs (includes MCP debug info)
- `docker compose down` – stop and remove all containers

The deployment exposes:

- `https://<host>/` – static landing page (see `nginx/www/index.html`)
- `https://<host>/app/` – proxied Streamlit UI
- MCP SSE endpoint available internally at `http://mcp:8765/mcp`

## Local development tips

- Review `streamlit/README.md` for instructions on running the UI outside
  Docker and for a tour of the application features.
- The MCP helper scripts log verbosely at DEBUG level; check the Streamlit
  container logs if tool calls appear to hang.
- Update the dark theme in `streamlit/app.py` if you tweak the landing page so
  both surfaces remain visually aligned.

## Maintenance

- The MCP Dockerfile installs `@fluree/fluree-mcp-server@latest` by default.
  Pin the `MCP_VERSION` build argument to a specific version to control upgrades.
- Nginx reads production configuration from `nginx/prod.conf`. Adjust upstreams
  or TLS settings there.
- Remember to renew certificates mounted at `/etc/letsencrypt` using your
  preferred ACME client on the host system.
