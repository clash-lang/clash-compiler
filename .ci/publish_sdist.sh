#!/bin/bash
set -eu -o pipefail

PASSWORD=$(echo $HACKAGE_PASSWORD | base64 --decode --ignore-garbage)

SDIST=$(find . -type f -regex "\./$1-[0-9\.]+\.tar\.gz")
DDIST=$(find . -type f -regex "\./$1-[0-9\.]+-docs\.tar\.gz")

set +u

if [[ "$HACKAGE_RELEASE" == "yes" ]]; then
    # Release tag set, upload as release.
    cabal upload --publish --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST}
    cabal upload --publish --documentation --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${DDIST}
elif [[ "$HACKAGE_RELEASE" == "no" ]]; then
    # Upload as release candidate
    cabal upload --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST}
    cabal upload --documentation --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${DDIST}
else
    echo "Unrecognized \$HACAKGE_RELEASE: $HACAKGE_RELEASE"
    exit 1;
fi
