#!/bin/sh
set -euo pipefail

nix-env -iA cachix -f https://cachix.org/api/v1/install

cachix use clash-lang
