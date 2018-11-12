#!/bin/bash
set -xeo pipefail
DIR=`dirname "$0"`
docker build -t clash-ci-image "$DIR"
docker tag clash-ci-image leonschoorl/clash-ci-image:trusty
docker push leonschoorl/clash-ci-image:trusty
