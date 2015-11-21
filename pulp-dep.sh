#!/bin/bash
DIR="$(dirname $(readlink -f "${BASH_SOURCE[0]}"))"
NODE_PATH=$DIR/output:$NODE_PATH node -e 'require("Main").main()' "$@"
