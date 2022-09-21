#!/bin/bash
set -xeo pipefail

apt update
apt install wget -y
wget -q https://get.haskellstack.org/ -O stack_install.sh
sh stack_install.sh
# Note: the --pedantic switch adds -Wall -Werror to the options passed to GHC.
# Options specified in stack.yaml (like -Wcompat) are still passed; it is
# cumulative. Future versions of Stack might add behavior to --pedantic.
stack build -j${THREADS} --pedantic
