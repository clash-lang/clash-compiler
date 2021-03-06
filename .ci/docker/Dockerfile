# syntax=docker/dockerfile:1.2

# Using buildkit we can build the different dependencies for CI independently,
# meaning upgrading one dependency won't force the others to be rebuilt. As a
# bonus, the host running 'docker build' will build the stages in parallel.
#
# To use buildkit, you need to set DOCKER_BUILDKIT=1 in your shell

ARG UBUNTU_VERSION=focal-20210416
FROM ubuntu:$UBUNTU_VERSION AS builder

LABEL vendor="QBayLogic B.V." maintainer="devops@qbaylogic.com"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PREFIX=/opt

ARG DEPS_COMMON="autoconf bison build-essential ca-certificates clang curl flex gcc g++ git make zlib1g-dev"
RUN apt-get update \
 && apt-get install -y --no-install-recommends $DEPS_COMMON

FROM builder AS build-ghdl

ARG DEPS_GHDL="gnat llvm-11-dev"
RUN apt-get install -y --no-install-recommends $DEPS_GHDL

ARG ghdl_version="1.0.0"
RUN curl -L "https://github.com/ghdl/ghdl/archive/v$ghdl_version.tar.gz" \
        | tar -xz \
 && cd ghdl-$ghdl_version \
 && ./configure --with-llvm-config=llvm-config-11 --prefix=$PREFIX \
 && make -j$(nproc) \
 && make install

FROM builder AS build-iverilog

ARG DEPS_IVERILOG="gperf"
RUN apt-get install -y --no-install-recommends $DEPS_IVERILOG

ARG iverilog_version="10_3"
RUN curl -L "https://github.com/steveicarus/iverilog/archive/v$iverilog_version.tar.gz" \
        | tar -xz \
 && cd iverilog-$iverilog_version \
 && sh autoconf.sh \
 && ./configure --prefix=$PREFIX \
 && make -j$(nproc) \
 && make install

FROM builder AS build-symbiyosys

ARG DEPS_YOSYS="libffi-dev libreadline-dev pkg-config tcl-dev"
ARG DEPS_Z3="python3 python3-distutils"
RUN apt-get install -y --no-install-recommends $DEPS_YOSYS $DEPS_Z3

ARG yosys_version="8cfed1a97957e4c096d1e0a0304d978bcb27e116"
RUN git clone https://github.com/YosysHQ/yosys.git yosys \
 && cd yosys \
 && git checkout $yosys_version \
 && make PREFIX=$PREFIX -j$(nproc) \
 && make PREFIX=$PREFIX install \
 && cd .. \
 && rm -Rf yosys

ARG z3_version="z3-4.8.10"
RUN curl -L "https://github.com/Z3Prover/z3/archive/refs/tags/$z3_version.tar.gz" \
      | tar -xz \
 && cd z3-$z3_version \
 && python3 scripts/mk_make.py \
 && cd build \
 && make PREFIX=$PREFIX -j$(nproc) \
 && make PREFIX=$PREFIX install \
 && cd ../.. \
 && rm -Rf z3-$z3_version

ARG symbiyosys_version="66a458958dc93f8e12418d425e4c31848889937b"
RUN git clone https://github.com/cliffordwolf/SymbiYosys.git SymbiYosys \
 && cd SymbiYosys \
 && git checkout $symbiyosys_version \
 && make PREFIX=$PREFIX -j$(nproc) install \
 && cd .. \
 && rm -Rf SymbiYosys

FROM builder AS build-ghc

ARG ghcup_version="0.1.15.2"

# Must be explicitly set
ARG ghc_version
ARG cabal_version

RUN curl "https://downloads.haskell.org/~ghcup/$ghcup_version/x86_64-linux-ghcup-$ghcup_version" --output /usr/bin/ghcup \
 && chmod +x /usr/bin/ghcup \
 && ghcup install ghc $ghc_version --set \
 && ghcup install cabal $cabal_version --set

ARG UBUNTU_VERSION=focal-20210416
FROM ubuntu:$UBUNTU_VERSION AS run

LABEL vendor="QBayLogic B.V." maintainer="devops@qbaylogic.com"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH="$PATH:/opt/bin:/root/.ghcup/bin"

ARG DEPS_RUNTIME="ca-certificates curl gcc git jq libc6-dev libgmp10-dev libgnat-9 libllvm11 libreadline8 libtinfo-dev libtcl8.6 make python3 ssh zlib1g-dev zstd"
RUN apt-get update \
 && apt-get install -y --no-install-recommends $DEPS_RUNTIME \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build-ghdl /opt /opt
COPY --from=build-iverilog /opt /opt
COPY --from=build-symbiyosys /opt /opt
COPY --from=build-ghc /root/.ghcup /root/.ghcup
