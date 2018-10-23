set -x
set -e
# test that source-distributions can be generated
(cd "./clash-lib" && cabal sdist && cd "..")
mv "."/clash-lib/dist/clash-lib-*.tar.gz ${DISTDIR}/
(cd "./clash-ghc" && cabal sdist && cd "..")
mv "."/clash-ghc/dist/clash-ghc-*.tar.gz ${DISTDIR}/
find ${DISTDIR} -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' -C ${DISTDIR} \;
sed -i 's/flags: cosim/flags: travisci cosim/g' cabal.project
cat cabal.project
# build & run tests, build benchmarks
cabal new-build all
if [ "x$TEST" = "x--enable-tests" ]; then cabal new-run -- clash-testsuite; fi
set +e
set +x
