#!/bin/bash
set -xuo pipefail

# Check whether version numbers in
# clash-{prelude{,-hedgehog},lib{,-hedgehog},ghc} are the same
cabal_files="clash-prelude/clash-prelude.cabal clash-prelude-hedgehog/clash-prelude-hedgehog.cabal clash-lib/clash-lib.cabal clash-lib-hedgehog/clash-lib-hedgehog.cabal clash-ghc/clash-ghc.cabal"
versions=$(grep "^[vV]ersion" $cabal_files | grep -Eo '[0-9]+(\.[0-9]+)+')
[[ $? -ne 0 ]] && exit 1

if [[ $(echo $versions | tr ' ' '\n' | wc -l) == 5 ]]; then
    if [[ $(echo $versions | tr ' ' '\n' | uniq | wc -l) != 1 ]]; then
        echo "Expected all distributions to have the same version number. Found: $versions"
        exit 1
    fi
else
    echo "Expected to find version number in all distributions. Found: $versions"
    exit 1
fi

if [[ $GITHUB_REF_TYPE == tag ]]; then
    # A tag has been created on GitHub. We use this to trigger a release
    # pipeline (release to Hackage).

    # You'd think comparing v${version} with ${GITHUB_REF_NAME} would do the
    # trick, but no..
    version=$(echo $versions | tr ' ' '\n' | head -n 1)
    tag_version=${GITHUB_REF_NAME:1}  # Strip first character (v0.99 -> 0.99)

    if [[ $version != $tag_version ]]; then
        if [[ ${GITHUB_REF_NAME:0:1} == v ]]; then
            echo "Tag name and distribution's release number should match:"
            echo "  Tag version:          $GITHUB_REF_NAME"
            echo "  Distribution version: v$version"
            exit 1
        else
            echo "\$GITHUB_REF_NAME should start with a 'v'. Found: $GITHUB_REF_NAME"
            exit 1
        fi
    fi
fi
