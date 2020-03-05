#!/bin/bash
set -xeo pipefail

apt update
apt install wget -y
wget -q https://get.haskellstack.org/ -O stack_install.sh
sh stack_install.sh
stack build -j${THREADS} --flag clash-prelude:-large-tuples
