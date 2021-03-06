FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8

# Caches dependencies of scripts & CI
ARG DEPS_SCRIPTS="devscripts cabal-install cabal-debian haskell-devscripts haskell-stack python3 equivs zstd"
ARG DEPS_SYMBIYOSYS="pkg-config clang tclsh git python python3 build-essential bison flex libreadline-dev gawk tcl-dev libffi-dev autoconf cmake"
RUN apt-get update && apt-get install -y $DEPS_SCRIPTS $DEPS_SYMBIYOSYS

ARG YOSYS_VERSION="8cfed1a97957e4c096d1e0a0304d978bcb27e116"
RUN git clone https://github.com/YosysHQ/yosys.git yosys \
 && cd yosys \
 && git checkout $YOSYS_VERSION \
 && make -j$(nproc) \
 && make install \
 && cd .. \
 && rm -Rf yosys

ARG Z3_VERSION="z3-4.8.10"
RUN curl -L "https://github.com/Z3Prover/z3/archive/refs/tags/$Z3_VERSION.tar.gz" \
      | tar -xz \
 && cd z3-$Z3_VERSION \
 && python scripts/mk_make.py \
 && cd build \
 && make -j$(nproc) \
 && make install \
 && cd ../.. \
 && rm -Rf z3-$Z3_VERSION

ARG SYMBIYOSYS_VERSION="66a458958dc93f8e12418d425e4c31848889937b"
RUN git clone https://github.com/cliffordwolf/SymbiYosys.git SymbiYosys \
 && cd SymbiYosys \
 && git checkout $SYMBIYOSYS_VERSION \
 && make install \
 && cd .. \
 && rm -Rf SymbiYosys

# First apt-get is the transitive dependencies of clash-ghc-{dev,prof}
# Second apt-get is the result of scanning a build log for "newly installed packages"
#
# Note that we don't need these, they're there purely to speed up the build process on CI.
RUN apt-get install -y binutils binutils-common binutils-x86-64-linux-gnu cpp cpp-9 dpkg gcc gcc-10-base gcc-9 gcc-9-base ghc ghc-prof libacl1 libasan5 libatomic1 libbinutils libbsd-dev libbsd0 libbz2-1.0 libc-dev-bin libc6 libc6-dev libcc1-0 libcrypt-dev libcrypt1 libctf-nobfd0 libctf0 libffi-dev libffi7 libgcc-9-dev libgcc-s1 libghc-adjunctions-dev libghc-adjunctions-prof libghc-aeson-dev libghc-aeson-pretty-dev libghc-aeson-pretty-prof libghc-aeson-prof libghc-ansi-terminal-dev libghc-ansi-terminal-prof libghc-ansi-wl-pprint-dev libghc-ansi-wl-pprint-prof libghc-arrows-dev libghc-arrows-prof libghc-attoparsec-dev libghc-attoparsec-prof libghc-base-compat-dev libghc-base-compat-prof libghc-base-orphans-dev libghc-base-orphans-prof libghc-base16-bytestring-dev libghc-base16-bytestring-prof libghc-bifunctors-dev libghc-bifunctors-prof libghc-blaze-builder-dev libghc-blaze-builder-prof libghc-blaze-html-dev libghc-blaze-html-prof libghc-blaze-markup-dev libghc-blaze-markup-prof libghc-call-stack-dev libghc-call-stack-prof libghc-charset-dev libghc-charset-prof libghc-clock-dev libghc-clock-prof libghc-colour-dev libghc-colour-prof libghc-comonad-dev libghc-comonad-prof libghc-constraints-dev libghc-constraints-prof libghc-contravariant-dev libghc-contravariant-prof libghc-cryptohash-sha256-dev libghc-cryptohash-sha256-prof libghc-data-binary-ieee754-dev libghc-data-binary-ieee754-prof libghc-data-default-class-dev libghc-data-default-class-prof libghc-data-default-dev libghc-data-default-instances-containers-dev libghc-data-default-instances-containers-prof libghc-data-default-instances-dlist-dev libghc-data-default-instances-dlist-prof libghc-data-default-instances-old-locale-dev libghc-data-default-instances-old-locale-prof libghc-data-default-prof libghc-distributive-dev libghc-distributive-prof libghc-dlist-dev libghc-dlist-prof libghc-erf-dev libghc-erf-prof libghc-exceptions-dev libghc-exceptions-prof libghc-extra-dev libghc-extra-prof libghc-fingertree-dev libghc-fingertree-prof libghc-first-class-families-dev libghc-first-class-families-prof libghc-free-dev libghc-free-prof libghc-ghc-paths-dev libghc-ghc-paths-prof libghc-half-dev libghc-half-prof libghc-hashable-dev libghc-hashable-prof libghc-haskell-lexer-dev libghc-haskell-lexer-prof libghc-hint-dev libghc-hint-prof libghc-integer-logarithms-dev libghc-integer-logarithms-prof libghc-interpolate-dev libghc-interpolate-prof libghc-invariant-dev libghc-invariant-prof libghc-kan-extensions-dev libghc-kan-extensions-prof libghc-lazysmallcheck-dev libghc-lazysmallcheck-prof libghc-lens-dev libghc-lens-prof libghc-old-locale-dev libghc-old-locale-prof libghc-parallel-dev libghc-parallel-prof libghc-parsers-dev libghc-parsers-prof libghc-pretty-show-dev libghc-pretty-show-prof libghc-prettyprinter-dev libghc-prettyprinter-prof libghc-primitive-dev libghc-primitive-prof libghc-profunctors-dev libghc-profunctors-prof libghc-quickcheck2-dev libghc-quickcheck2-prof libghc-random-dev libghc-random-prof libghc-recursion-schemes-dev libghc-recursion-schemes-prof libghc-reducers-dev libghc-reducers-prof libghc-reflection-dev libghc-reflection-prof libghc-safe-dev libghc-safe-prof libghc-scientific-dev libghc-scientific-prof libghc-semigroupoids-dev libghc-semigroupoids-prof libghc-semigroups-dev libghc-semigroups-prof libghc-singletons-dev libghc-singletons-prof libghc-split-dev libghc-split-prof libghc-src-exts-dev libghc-src-exts-prof libghc-src-meta-dev libghc-src-meta-prof libghc-statevar-dev libghc-statevar-prof libghc-stream-dev libghc-stream-prof libghc-syb-dev libghc-syb-prof libghc-tagged-dev libghc-tagged-prof libghc-temporary-dev libghc-temporary-prof libghc-terminal-size-dev libghc-terminal-size-prof libghc-tf-random-dev libghc-tf-random-prof libghc-th-abstraction-dev libghc-th-abstraction-prof libghc-th-desugar-dev libghc-th-desugar-prof libghc-th-expand-syns-dev libghc-th-expand-syns-prof libghc-th-lift-dev libghc-th-lift-instances-dev libghc-th-lift-instances-prof libghc-th-lift-prof libghc-th-orphans-dev libghc-th-orphans-prof libghc-th-reify-many-dev libghc-th-reify-many-prof libghc-time-locale-compat-dev libghc-time-locale-compat-prof libghc-transformers-base-dev libghc-transformers-base-prof libghc-transformers-compat-dev libghc-transformers-compat-prof libghc-trifecta-dev libghc-trifecta-prof libghc-type-errors-dev libghc-type-errors-prof libghc-uniplate-dev libghc-uniplate-prof libghc-unordered-containers-dev libghc-unordered-containers-prof libghc-utf8-string-dev libghc-utf8-string-prof libghc-uuid-types-dev libghc-uuid-types-prof libghc-vector-binary-instances-dev libghc-vector-binary-instances-prof libghc-vector-dev libghc-vector-prof libghc-void-dev libghc-void-prof libgmp-dev libgmp10 libgmpxx4ldbl libgomp1 libisl22 libitm1 liblsan0 liblzma5 libmpc3 libmpfr6 libncurses-dev libncurses5-dev libncurses6 libncursesw6 libpcre2-8-0 libquadmath0 libselinux1 libstdc++6 libtinfo6 libtsan0 libubsan1 libzstd1 linux-libc-dev ncurses-bin tar zlib1g \
 && apt-get install -y libghc-base-orphans-doc libghc-bifunctors-doc libghc-comonad-doc libghc-distributive-doc libghc-profunctors-doc libghc-tagged-doc libghc-th-abstraction-doc libghc-unbounded-delays-dev libghc-unbounded-delays-doc libghc-unbounded-delays-prof doctest libghc-code-page-dev libghc-doctest-dev libpthread-stubs0-dev libghc-async-dev libghc-async-prof libghc-base-prelude-dev libghc-base-prelude-prof libghc-case-insensitive-dev libghc-case-insensitive-prof libghc-glob-dev libghc-glob-prof libghc-megaparsec-dev libghc-megaparsec-prof libghc-neat-interpolation-dev libghc-neat-interpolation-prof libghc-optparse-applicative-dev libghc-optparse-applicative-prof libghc-parser-combinators-dev libghc-parser-combinators-prof libghc-tasty-dev libghc-tasty-hunit-dev libghc-tasty-hunit-prof libghc-tasty-prof libghc-wcwidth-dev libghc-wcwidth-prof ghdl ghdl-llvm libgnat-9 libllvm10 zlib1g-dev libghc-base-orphans-doc libghc-bifunctors-doc libghc-comonad-doc libghc-distributive-doc libghc-profunctors-doc libghc-tagged-doc libghc-th-abstraction-doc \
 && cabal update
