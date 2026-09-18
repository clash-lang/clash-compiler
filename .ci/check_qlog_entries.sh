#!/usr/bin/env bash

set -uo pipefail

# List added/modified files in changelog directory
gh pr view "$1" --json files --jq '
    .files[] | select(.changeType != "DELETED") | .path |
    select(test("^changelog/entries/"))
    ' |
  xargs -I{} nix run .#devTools.qlog -- check -t {} || exit $?
