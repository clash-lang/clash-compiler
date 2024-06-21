#!/usr/bin/env bash
set -euo pipefail

# Help message
show_help() {
    local script_name
    script_name=$(basename "$0")
    echo "Fourmolu formatting Script.

Usage:
  $script_name diff    Format the current git diff.
  $script_name full    Format all source files.
  $script_name check   Check the formatting of the source files.
  $script_name (-h | --help)

Options:
  -h --help     Show this screen."
}

# Main script logic
if [[ "$#" -eq 0 || "$1" == "-h" || "$1" == "--help" ]]; then
    show_help
    exit 0
fi

# use array so we can easily append
# Ignore the clash-ghc/src-bin-* since they basically copies of upstream GHC code
readarray -d '' exclude_files < <(find ./clash-ghc/src-bin-* -type f -name "*.hs" -print0)

# Make sure it doesn't matter from where this script is executed
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

if [ $1 == "diff" ] ; then
  src_files=$(git diff --cached --name-only --diff-filter=ACMR -- '*.hs')
else
  src_files=$(find ./* -type f -name "*.hs")
fi

src_files_str=$(printf "%s\n" "${src_files[@]}" | sed 's| |\\ |g')
exclude_files_str=$(printf "%s\n" "${exclude_files[@]}" | sed 's| |\\ |g')
src_files=$(echo "$src_files_str" | grep -vwE  "$exclude_files_str")

if [ -z "$src_files" ]; then
  exit 0;
fi

if [[ "$1" == "diff" || "$1" == "full" ]] ; then
  fourmolu --mode inplace $src_files
elif [[ "$1" == "check" ]] ; then
  fourmolu --mode check $src_files
else
  echo "Expected a single argument, \"full\", \"diff\", \"check\" or \"--help\"" >&2
  exit 3
fi
