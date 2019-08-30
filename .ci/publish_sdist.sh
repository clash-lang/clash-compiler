#!/bin/bash
set -eu -o pipefail

PASSWORD=$(echo $HACKAGE_PASSWORD | base64 --decode --ignore-garbage)

SDIST=$(find $1-*.tar.gz | grep -v docs)
DDIST=$(find $1-*.tar.gz | grep docs)

set +u

if [[ "$HACKAGE_RELEASE" == "yes" ]]; then
    # Release tag set, upload as release.
    # TODO: Right now, this branch will do the same as the other: make a
    # release candidate. We can change it to actually have it release after
    # we've gained enough trust in the release logic.
    echo "XXX: Release logic activated!"
    cabal upload --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST} 
    cabal upload --documentation --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${DDIST} 
elif [[ "$HACKAGE_RELEASE" == "no" ]]; then
    # Upload as release candidate
    cabal upload --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST} 
    cabal upload --documentation --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${DDIST} 
else
    echo "Unrecognized \$HACAKGE_RELEASE: $HACAKGE_RELEASE"
    exit 1;
fi
