#!/usr/bin/env bash
# Negative compile tests of scoped Clash Core ("Clash.Core").
#
# Every N*.hs is a buggy use of the API, of a kind that caused scoping bugs in
# Clash, that GHC must reject with the error that its "-- EXPECT:" line gives
# (an extended regular expression, matched against the error messages joined
# into one line, with ASCII quotes). Ok.hs has the correct versions, and must
# typecheck: that checks that the negative tests fail for the reason they are
# meant to.
#
# Run after building clash-lib, passing the cabal flags of that build, e.g.:
#
#   clash-lib/tests/scoped-negative/check.sh
#   clash-lib/tests/scoped-negative/check.sh -w ghc-9.6.7 --builddir=dist-9.6
set -uo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$DIR/../../.." && pwd)"
OUT="$(mktemp -d)"
trap 'rm -rf "$OUT"' EXIT

# Typecheck a file in the environment of the cabal project. Prints the output
# of GHC, with its messages joined into one line and with ASCII quotes.
typecheck() {
  local file="$1"
  shift
  (cd "$ROOT" && cabal exec -v0 "$@" -- \
    ghc -fno-code -fforce-recomp -package clash-lib -outputdir "$OUT" "$file" 2>&1) \
    | sed -e "s/‘/'/g" -e "s/’/'/g" | tr '\n' ' ' | tr -s ' '
}

status=0

echo "=== Ok.hs: must typecheck"
if out=$(typecheck "$DIR/Ok.hs" "$@"); then
  echo "ok"
else
  echo "!! does not typecheck:"
  echo "$out"
  exit 1
fi

for f in "$DIR"/N*.hs; do
  name=$(basename "$f")
  expect=$(sed -n 's/^-- EXPECT: //p' "$f" | sed -e "s/‘/'/g" -e "s/’/'/g")
  echo "=== $name: must fail with: $expect"
  if out=$(typecheck "$f" "$@"); then
    echo "!! typechecks, but should not"
    status=1
  elif grep -qE -- "$expect" <<< "$out"; then
    echo "ok"
  else
    echo "!! fails, but not with the expected error:"
    echo "$out"
    status=1
  fi
done

exit $status
