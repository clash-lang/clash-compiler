#!/bin/bash
set -eu -o pipefail

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
    cabal upload --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST}
    cabal upload --documentation --token=${HACKAGE_TOKEN} ${DDIST}
else
    echo "Unrecognized \$HACKAGE_RELEASE: $HACKAGE_RELEASE"
    exit 1;
fi
