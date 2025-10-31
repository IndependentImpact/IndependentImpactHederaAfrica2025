#!/usr/bin/env sh
set -eu

TRANSPORT="${TRANSPORT:-sse}"
BIND_ADDRESS="${BIND_ADDRESS:-0.0.0.0}"
PORT="${PORT:-8765}"
FLUREE_BASE_URL="${FLUREE_BASE_URL:-https://datadudes2.xyz/fluree}"
DATASET="${DATASET:-}"
DATASETS="${DATASETS:-}"
FLUREE_API_KEY="${FLUREE_API_KEY:-}"
DATASET_POLICIES="${DATASET_POLICIES:-}"

# Prefer Docker secret if present
if [ -f "/run/secrets/fluree_api_key" ] && [ -z "${FLUREE_API_KEY}" ]; then
  FLUREE_API_KEY="$(cat /run/secrets/fluree_api_key)"
fi

ARGS="--transport ${TRANSPORT} --bind-address ${BIND_ADDRESS} --port ${PORT} --fluree-base-url ${FLUREE_BASE_URL}"

[ -n "$DATASET" ] && ARGS="$ARGS --dataset $DATASET"
if [ -n "$DATASETS" ]; then
  IFS=','; set -- $DATASETS; IFS=' '
  for d in "$@"; do ARGS="$ARGS --dataset $d"; done
fi
[ -n "$FLUREE_API_KEY" ] && ARGS="$ARGS --fluree-api-key $FLUREE_API_KEY"
if [ -n "$DATASET_POLICIES" ]; then
  IFS=';'; set -- $DATASET_POLICIES; IFS=' '
  for p in "$@"; do ARGS="$ARGS --dataset-policy $p"; done
fi
[ -n "${EXTRA_ARGS:-}" ] && ARGS="$ARGS $EXTRA_ARGS"

echo "Starting fluree-mcp-server $ARGS"
exec fluree-mcp-server $ARGS
