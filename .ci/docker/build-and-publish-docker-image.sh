#!/bin/bash
set -xeo pipefail
DIR=`dirname "$0"`
now=$(date)
docker build -t clash-ci-image --build-arg "GHC_FETCH_DATE=$now" "$DIR"
echo Press enter to upload image to dockerhub...
read
docker tag clash-ci-image leonschoorl/clash-ci-image:trusty
docker push leonschoorl/clash-ci-image:trusty
