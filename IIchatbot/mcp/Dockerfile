FROM node:20-alpine

ARG MCP_VERSION=@fluree/fluree-mcp-server@latest
RUN npm install -g ${MCP_VERSION}

WORKDIR /app
COPY entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh

EXPOSE 8765
ENTRYPOINT ["/app/entrypoint.sh"]
