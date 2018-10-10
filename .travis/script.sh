set -x
set -e
# test that source-distributions can be generated
(cd "./clash-lib" && cabal sdist && cd "..")
mv "."/clash-lib/dist/clash-lib-*.tar.gz ${DISTDIR}/
(cd "./clash-ghc" && cabal sdist && cd "..")
mv "."/clash-ghc/dist/clash-ghc-*.tar.gz ${DISTDIR}/
find ${DISTDIR} -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' -C ${DISTDIR} \;
printf 'packages: %b/clash-lib-*/*.cabal %b/clash-ghc-*/*.cabal clash-prelude/*.cabal clash-cosim/*.cabal testsuite/*.cabal benchmark/*.cabal benchmark/profiling/*/*.cabal\npackage clash-testsuite\n  flags: travisci cosim\n\npackage clash-ghc\n  executable-dynamic: True\n' ${DISTDIR} ${DISTDIR} > cabal.project
cat cabal.project
# this builds all libraries and executables 
cabal new-build -w ${HC} all

# Build with installed constraints for packages in global-db
if $INSTALLED; then echo cabal new-build -w ${HC} $(${HCPKG} list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all | sh; else echo "Not building with installed constraints"; fi

# cabal check
(cd ${DISTDIR}/clash-lib-* && cabal check)
(cd ${DISTDIR}/clash-ghc-* && cabal check)

# haddock
cd ${SRCDIR}
if $HADDOCK; then cabal new-haddock -w ${HC} ${TEST} ${BENCH} all; else echo "Skipping haddock generation";fi
set +e
set +x
