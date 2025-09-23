#!/bin/sh
set -euo pipefail

nix \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  build -j$THREADS --log-format raw --max-silent-time 3600 "$@"

nix \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  build --no-link --print-out-paths "$@" | cachix push clash-lang
