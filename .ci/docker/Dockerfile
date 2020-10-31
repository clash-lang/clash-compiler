FROM ubuntu:bionic

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/root/.ghcup/bin:$PATH

ARG DEPS_GHC="curl libc6-dev libgmp-dev pkg-config libnuma-dev"
ARG DEPS_CABAL="zlib1g-dev"
ARG DEPS_GHDL="clang gcc gnat llvm-dev"
ARG DEPS_IVERILOG="autoconf bison flex gperf"
ARG DEPS_CLASH="libtinfo-dev"
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
      ca-certificates \
      git ssh \
 && curl -L 'https://github.com/ghdl/ghdl/archive/v0.36.tar.gz' \
      | tar -xz \
 && curl -L 'https://github.com/steveicarus/iverilog/archive/v10_3.tar.gz' \
      | tar -xz

WORKDIR /ghdl-0.36/build

RUN ../configure --with-llvm-config --prefix=/opt \
 && make -j$(nproc) \
 && make install

WORKDIR /iverilog-10_3

RUN chmod a+x autoconf.sh \
 && ./autoconf.sh \
 && ./configure --prefix=/opt \
 && make -j$(nproc) \
 && make install

WORKDIR /

RUN apt-get remove -y $DEPS_IVERILOG \
 && rm -Rf ghdl-0.36 iverilog-10_3 \
 && apt-get update \
 && apt-get autoremove -y --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

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
