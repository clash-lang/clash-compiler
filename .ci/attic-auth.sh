#!/bin/bash

set -uo pipefail

mkdir -p ~/.config/attic
cat <<EOF > ~/.config/attic/config.toml
default-server = "public"

[servers.public]
endpoint = "http://diepenheim.local:9200"
token = "$ATTIC_AUTH_TOKEN"
EOF
