#!/usr/bin/env bash

set -xeo pipefail

REPO="ghcr.io/clash-lang"
NAME="snapcraft"

DIR=$(dirname "$0")
now=$(date +%F)

docker build -t "${REPO}/${NAME}:$now" "$DIR"
docker tag "${REPO}/${NAME}:$now" "${REPO}/${NAME}:latest"

read -p "Push to GitHub? (y/N) " push

if [[ $push =~ ^[Yy]$ ]]; then
        docker push "${REPO}/${NAME}:$now"
        docker push "${REPO}/${NAME}:latest"
else
        echo "Skipping push to container registry"
fi
