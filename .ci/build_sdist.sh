#!/bin/bash
set -x -euf -o pipefail

cd $(dirname $0)/..

# Enable documentation building for all dependencies
sed -i 's/documentation: False/documentation: True/g' cabal.project.local

# Build source distribution
cabal sdist $1 |& tee sdist.log

# Build documentation
cabal new-haddock $1 \
    --haddock-for-hackage \
    --haddock-hyperlinked-source \
    --enable-documentation |& tee haddock.log

# Extract location of sdist tarball
SDIST=$(grep "Wrote tarball sdist to" -A 1 sdist.log | tail -n 1)

# Extract location of documentation tarball
DDIST=$(grep "Documentation tarball created:" -A 1 haddock.log | tail -n 1)

# Copy
cd -
cp $SDIST $(basename $SDIST)  # Example: clash-prelude-0.9999.tar.gz
cp $DDIST $(basename $DDIST)  # Example: clash-prelude-0.9999-docs.tar.gz
