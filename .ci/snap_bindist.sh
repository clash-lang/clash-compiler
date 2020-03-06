#!/bin/bash
set -euo pipefail
# TODO: Clean up nix-store after building

THREADS=$(nproc)
export THREADS
export XZ_DEFAULTS="-T ${THREADS} -3"
ROOT=$(pwd)
export ROOT

# Restore cache (gitlab ci's method is too slow for big caches)
tar xf usr_nix.tar.xz -C / || true

# Build binary distribution
# TODO: Remove need for ksh in mkBinDist.sh
apt update
apt install ksh -y
cd bindist/linux/snap
./mkBinDist.sh |& tee $ROOT/nix_build.log | (egrep -i '^(building|copying)' || true)

# Create cache
tar cJf $ROOT/usr_nix.tar.xz /usr/nix
