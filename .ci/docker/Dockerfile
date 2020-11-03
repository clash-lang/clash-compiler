FROM ubuntu:focal

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/root/.ghcup/bin:$PATH

ARG DEPS_GHC="curl libc6-dev libgmp-dev pkg-config libnuma-dev"
ARG DEPS_CABAL="zlib1g-dev"
ARG DEPS_GHDL="clang gcc gnat llvm-9-dev"
ARG DEPS_CLASH="libtinfo-dev libtinfo5"
ARG DEPS_CLASH_COSIM="make"

ARG GHCUP_URL="https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup"
ARG GHCUP_BIN=/usr/bin/ghcup

RUN apt-get update \
 # GHDL compilation fails without dist-upgrade
 && apt-get dist-upgrade -y \
 && apt-get install -y --no-install-recommends --no-install-suggests \
      $DEPS_GHC $DEPS_CABAL \
      $DEPS_GHDL $DEPS_IVERILOG \
      $DEPS_CLASH $DEPS_CLASH_COSIM \
      ca-certificates iverilog \
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

WORKDIR /

RUN curl $GHCUP_URL --output $GHCUP_BIN \
 && chmod +x $GHCUP_BIN \
 && ghcup upgrade \
 && ghcup install ghc 8.4.4 \
 && ghcup install ghc 8.6.5 \
 && ghcup install ghc 8.8.4 \
 && ghcup install ghc 8.10.2 \
 && ghcup install ghc 9.0.0.20200925 \
 && ghcup install cabal 2.4.1.0 \
 && ghcup install cabal 3.0.0.0 \
 && ghcup install cabal 3.2.0.0 \
 && ghcup install cabal 3.4.0.0-rc4
