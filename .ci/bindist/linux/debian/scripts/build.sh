#!/bin/bash
# Bootstrap Python installation and run build.py
set -x -euf -o pipefail

cd $(dirname $0)
apt-get update
apt-get install python3 -y
./build.py $@
