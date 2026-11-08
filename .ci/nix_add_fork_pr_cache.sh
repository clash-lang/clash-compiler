#!/bin/sh

set -eu

mkdir -p ~/.config/nix
cat >~/.config/nix/nix.conf <<EOF
extra-substituters = http://192.168.102.136:9200/fork-pr/
extra-trusted-substituters = http://192.168.102.136:9200/fork-pr/
extra-trusted-public-keys = fork-pr:c/DC67RNFrg29tydCbZlUzSPNURZranTk4N0UHFbzBk=
EOF
