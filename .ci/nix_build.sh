#!/bin/bash
set -xeo pipefail

sed -z -i 's/-- large-tuples\n  default: True/default: False/' clash-prelude/clash-prelude.cabal || true
nix-build
