#!/bin/bash

set -uo pipefail

mkdir -p ~/.config/attic
cat <<EOF > ~/.config/attic/config.toml
default-server = "$ATTIC_CACHE"

[servers.$ATTIC_CACHE]
endpoint = "http://diepenheim:9200"
token = "$ATTIC_AUTH_TOKEN"
EOF
