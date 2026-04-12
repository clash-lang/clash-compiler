#!/bin/bash
set -eu -o pipefail

. .ci/functions.sh

trap exit_trap EXIT

PASSWORD=$(echo $HACKAGE_PASSWORD | base64 --decode --ignore-garbage)

SDIST=$(find . -type f -regex "\./$1-[0-9\.]+\.tar\.gz")
DDIST=$(find . -type f -regex "\./$1-[0-9\.]+-docs\.tar\.gz")

set +u

if [[ "$HACKAGE_RELEASE" == "yes" ]]; then
    # Release tag set, upload as release.
    cabal upload --publish --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST}
    cabal upload --publish --documentation --token=${HACKAGE_TOKEN} ${DDIST}
elif [[ "$HACKAGE_RELEASE" == "no" ]]; then
    # Upload as release candidate

    version=$(grep "^[vV]ersion" "$1/$1.cabal" | grep -Eo '[0-9]+(\.[0-9]+)+')

    if cabal list --simple-output "$1" | grep -q "^$1 $version$"; then
      echo "$1 v$version has been published, cannot upload to Hackage" >&2
      trap - EXIT
      # Signals allowed-to-fail to CI
      exit 64
    fi

    cabal upload --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST}
    cabal upload --documentation --token=${HACKAGE_TOKEN} ${DDIST}
else
    echo "Unrecognized \$HACKAGE_RELEASE: $HACKAGE_RELEASE"
    exit 1;
fi
