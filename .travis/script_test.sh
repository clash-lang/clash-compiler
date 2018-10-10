set -x
set -e
# test that source-distributions can be generated
(cd "./clash-lib" && cabal sdist && cd "..")
mv "."/clash-lib/dist/clash-lib-*.tar.gz ${DISTDIR}/
(cd "./clash-ghc" && cabal sdist && cd "..")
mv "."/clash-ghc/dist/clash-ghc-*.tar.gz ${DISTDIR}/
find ${DISTDIR} -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' -C ${DISTDIR} \;
printf 'packages: %b/clash-lib-*/*.cabal %b/clash-ghc-*/*.cabal clash-prelude/*.cabal clash-cosim/*.cabal testsuite/*.cabal\npackage clash-testsuite\n  flags: travisci cosim\n\npackage clash-ghc\n  executable-dynamic: True\n' ${DISTDIR} ${DISTDIR} > cabal.project
cat cabal.project
# build & run tests, build benchmarks
cabal new-build -w ${HC} all
if [ "x$TEST" = "x--enable-tests" ]; then cabal new-run -w ${HC} -- clash-testsuite -j16; fi
set +e
set +x
