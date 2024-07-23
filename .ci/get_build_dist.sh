#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

GIT_ROOT=$(git rev-parse --show-toplevel)
PLAN_JSON=${GIT_ROOT}/dist-newstyle/cache/plan.json

CABAL_HOME=${HOME}/.cabal
GHC_VERSION=$(ghc --version | grep -E -o '[0-9.]+$')
if ghc --info | grep -q "Project Unit Id"; then
  GHC_ABI=$(ghc --info | grep "Project Unit Id" | tail -c 7 | cut -c 1-4)
  PKG_STORE_DIR=${CABAL_HOME}/store/ghc-${GHC_VERSION}-${GHC_ABI}
else
  PKG_STORE_DIR=${CABAL_HOME}/store/ghc-${GHC_VERSION}
fi

# Extract packages used from install plan
pkgs=$(jq -r '."install-plan"[].id' "${PLAN_JSON}")

# Check which exists (filters global packages)
for pkg in ${pkgs}; do
  if [[ -d "${PKG_STORE_DIR}/${pkg}" ]]; then
    echo "${PKG_STORE_DIR}/${pkg}"
    conf_file="${PKG_STORE_DIR}/package.db/${pkg}.conf"
    if [[ -f "${conf_file}" ]]; then
      echo "${conf_file}"
    fi
  fi
done

echo "${GIT_ROOT}"/dist-newstyle
ls "${GIT_ROOT}"/.ghc.environment.*

# Pack source distribution too to prevent rebuilds due to changed modification
# dates.
ls -d "${GIT_ROOT}"/clash-*

# Include compile options
echo "${GIT_ROOT}"/cabal.project.local
