#!/usr/bin/env bash

set -xeo pipefail

REPO="ghcr.io/clash-lang"
NAME="clash-ci"
DIR=$(dirname "$0")
now=$(date +%Y%m%d)

if [[ "$1" == "-y" ]]; then
  push=y
elif [[ "$1" != "" ]]; then
  echo "Unrecognized argument: $1" >&2
  exit 1
fi

UBUNTU_VERSION=jammy-20240808
GHC_VERSIONS=("9.10.1" "9.8.4"  "9.6.6"  "9.4.8"  "9.2.8"   "9.0.2"   "8.10.7")
CABAL_VERSION="3.14.1.0"

# We want to use docker buildkit so that our layers are built in parallel. This
# is ignored completely on versions of docker which don't support buildkit.
export DOCKER_BUILDKIT=1

for i in "${!GHC_VERSIONS[@]}"
do
  GHC_VERSION="${GHC_VERSIONS[i]}"

  docker build \
    --build-arg UBUNTU_VERSION=${UBUNTU_VERSION} \
    --build-arg cabal_version=${CABAL_VERSION} \
    --build-arg ghc_version=${GHC_VERSION} \
    -t "${REPO}/${NAME}:${GHC_VERSION}-$now" \
    "$DIR"
done

if [[ "${push}" == "" ]]; then
  read -p "Push to GitHub? (y/N) " push
fi

if [[ $push =~ ^[Yy]$ ]]; then
  for i in "${!GHC_VERSIONS[@]}"
  do
    GHC_VERSION="${GHC_VERSIONS[i]}"
    docker push "${REPO}/${NAME}:${GHC_VERSION}-$now"
  done
else
  echo "Skipping push to container registry"
fi
