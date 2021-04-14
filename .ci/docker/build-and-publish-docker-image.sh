#!/usr/bin/env bash

set -xeo pipefail

REPO="docker.pkg.github.com/clash-lang/clash-compiler"
NAME="clash-ci-"
DIR=$(dirname "$0")
now=$(date +%F)

GHC_VERSIONS="9.0.1 8.10.4 8.8.4 8.6.5 8.4.4"
for GHC_VERSION in $GHC_VERSIONS
do
  docker build --build-arg ghc_version=${GHC_VERSION} -t "${REPO}/${NAME}${GHC_VERSION}:$now" "$DIR"
  docker tag "${REPO}/${NAME}${GHC_VERSION}:$now" "${REPO}/${NAME}${GHC_VERSION}:latest"
done


read -p "Push to GitHub? (y/N) " push

if [[ $push =~ ^[Yy]$ ]]; then
  for GHC_VERSION in $GHC_VERSIONS
  do
    docker push "${REPO}/${NAME}${GHC_VERSION}:$now"
    docker push "${REPO}/${NAME}${GHC_VERSION}:latest"
  done
else
  echo "Skipping push to container registry"
fi
