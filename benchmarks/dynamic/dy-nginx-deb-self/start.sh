#!/bin/bash
set -euo pipefail

ulimit -n 100000

python3 app.py &
APP_PID=$!

sleep 2

nginx -g "daemon off;" &
NGINX_PID=$!

cleanup() {
    kill "$APP_PID" "$NGINX_PID" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

# Exit the container as soon as either service exits.
wait -n "$APP_PID" "$NGINX_PID"
EXIT_CODE=$?
exit "$EXIT_CODE"