#!/usr/bin/env bash

set -xeo pipefail

DIR=$(dirname "$0")
now=$(date +%F)

docker build -t "clashlang/clash-ci:$now" "$DIR"
docker tag "clashlang/clash-ci:$now" "clashlang/clash-ci:latest"

read -p "Push to DockerHub? (y/N) " push

if [[ $push =~ ^[Yy]$ ]]; then
        docker push "clashlang/clash-ci:$now"
        docker push "clashlang/clash-ci:latest"
else
        echo "Skipping push to container registry"
fi

