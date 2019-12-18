#!/usr/bin/env bash

set -euo pipefail

SCRIPT=$(basename $0)

if [[ -z $(command -v python3) ]]; then
        echo "$SCRIPT: python3 is not installed"
        exit 127
fi

echo "$SCRIPT: Building HTML"
python3 -msphinx -M html . _build

echo "$SCRIPT: Building LaTeX"
python3 -msphinx -M latexpdf . _build

