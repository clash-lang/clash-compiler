FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/root/.ghcup/bin:$PATH

ARG DEPS_GHC="curl libc6-dev libgmp-dev pkg-config libnuma-dev"
ARG DEPS_CABAL="zlib1g-dev"
ARG DEPS_GHDL="clang gcc gnat llvm-9-dev"
ARG DEPS_CLASH="libtinfo-dev libtinfo5"
ARG DEPS_CLASH_COSIM="make"

RUN apt-get update \
 # GHDL compilation fails without dist-upgrade
 && apt-get dist-upgrade -y \
 && apt-get install -y --no-install-recommends --no-install-suggests \
      $DEPS_GHC $DEPS_CABAL \
      $DEPS_GHDL $DEPS_IVERILOG \
      $DEPS_CLASH $DEPS_CLASH_COSIM \
      ca-certificates iverilog pixz jq zstd \
      git ssh \
 && curl -L 'https://github.com/ghdl/ghdl/archive/v0.37.tar.gz' \
      | tar -xz \
 && apt-get autoremove -y --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && cd /ghdl-0.37 \
 && ./configure --with-llvm-config=llvm-config-9 --prefix=/opt \
 && make -j$(nproc) \
 && make install \
 && cd .. \
 && rm -rf ghdl-0.37

ARG GHCUP_VERSION="0.1.14"
ARG GHCUP_URL="https://downloads.haskell.org/~ghcup/${GHCUP_VERSION}/x86_64-linux-ghcup-${GHCUP_VERSION}"
ARG GHCUP_BIN=/usr/bin/ghcup

ARG cabal_version
ENV CABAL_VERSION=$cabal_version
RUN curl $GHCUP_URL --output $GHCUP_BIN \
 && chmod +x $GHCUP_BIN \
 && ghcup install cabal ${CABAL_VERSION} \
 && ghcup set cabal ${CABAL_VERSION}

ARG ghc_version
ENV GHC_VERSION=$ghc_version
RUN ghcup install ghc ${GHC_VERSION}
