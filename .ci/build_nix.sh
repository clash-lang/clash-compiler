#!/bin/sh

set -u

nix \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  build -j$THREADS --log-format raw --max-silent-time 3600 "$@"
