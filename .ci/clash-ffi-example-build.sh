#!/bin/sh
set -xeu

# See PR #2572. Undo mmap crash workaround for clash-ffi-example.
cd clash-ffi/example
cabal build clash-ffi
GHCRTS="" cabal build clash-ffi-example
