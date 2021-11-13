#!/bin/bash

set -euo pipefail

WHICH="${1:-}"

if [ "$WHICH" != "server" ] && [ "$WHICH" != "client" ]; then
    echo "Usage: $0 server | client"
    exit 1
fi
export TLSER_CERTS=ecc
export TLSER_START="$WHICH"
rebar3 shell
