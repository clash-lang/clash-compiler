#!/bin/bash
set -euo pipefail

cd $(dirname $0)

export SNAPCRAFT_BUILD_ENVIRONMENT_CPU=$(nproc)
export SNAPCRAFT_BUILD_ENVIRONMENT_MEMORY=8G

DEBIAN_DIR=../debian

#${DEBIAN_DIR}/go.sh focal
tar -cf focal-build.tar -C ${DEBIAN_DIR}/focal/build .
snapcraft clean
snapcraft
