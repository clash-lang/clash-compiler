#!/bin/bash
set -euo pipefail

SIMPLE_PATH=$(echo dist-newstyle/build/*/*/"$1"-*/"$3"/"$2"/build/"$2"/"$2")
CUSTOM_PATH=$(echo dist-newstyle/build/*/*/"$1"-*/build/"$2"/"$2")

if [ -x "$SIMPLE_PATH" ]; then
  echo "$SIMPLE_PATH"
elif [ -x "$CUSTOM_PATH" ]; then
  echo "$CUSTOM_PATH"
else
  echo "find_cabal_bin.sh: Could not find $1 $2" >/dev/stderr
fi
