#!/bin/bash
set -xeo pipefail

apt update
apt install wget -y
wget -qO- https://get.haskellstack.org/ | sh
stack build
