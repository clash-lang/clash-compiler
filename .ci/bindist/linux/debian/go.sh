#!/bin/bash
set -x -euf -o pipefail

cd $(dirname $0)

docker run \
  -v $(realpath ../../../..):/clash-compiler-src \
  $(cat $1/buildinfo.json | jq -r '.docker') \
  /clash-compiler-src/.ci/bindist/linux/debian/scripts/build.sh ${1}
