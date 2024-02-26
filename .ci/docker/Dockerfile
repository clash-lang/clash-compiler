# syntax=docker/dockerfile:1.2

# Using buildkit we can build the different dependencies for CI independently,
# meaning upgrading one dependency won't force the others to be rebuilt. As a
# bonus, the host running 'docker build' will build the stages in parallel.
#
# To use buildkit, you need to set DOCKER_BUILDKIT=1 in your shell

ARG UBUNTU_VERSION
FROM ubuntu:$UBUNTU_VERSION AS builder

LABEL vendor="QBayLogic B.V." maintainer="devops@qbaylogic.com"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PREFIX=/opt

ARG DEPS_COMMON="autoconf bison build-essential ca-certificates clang cmake curl flex gcc g++ git make zlib1g-dev"
RUN apt-get update \
 && apt-get install -y --no-install-recommends $DEPS_COMMON

FROM builder AS build-ghdl

ARG DEPS_GHDL="gnat-10 llvm-13-dev"
RUN apt-get install -y --no-install-recommends $DEPS_GHDL

ARG ghdl_version="2.0.0"
RUN curl -L "https://github.com/ghdl/ghdl/archive/v$ghdl_version.tar.gz" \
        | tar -xz \
 && cd ghdl-$ghdl_version \
 && ./configure --with-llvm-config=llvm-config-13 --prefix=$PREFIX \
 && make -j$(nproc) \
 && make install

FROM builder AS build-iverilog

ARG DEPS_IVERILOG="gperf"
RUN apt-get install -y --no-install-recommends $DEPS_IVERILOG

# At the time of writing (2023-04-14), the latest commit in 'v10-branch'. This
# commit is needed to work around compilation issues with newer 'bison's shipped
# with 22.04.
ARG iverilog_version="b988543096183650979e2aae9702de3750fee798"
RUN curl -L "https://github.com/steveicarus/iverilog/archive/${iverilog_version}.tar.gz" \
        | tar -xz \
 && cd iverilog-* \
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

ARG boolector_version="3.2.0"
RUN curl -L "https://github.com/Boolector/boolector/archive/refs/tags/$boolector_version.tar.gz" \
      | tar -xz \
 && cd boolector-$boolector_version \
 && ./contrib/setup-lingeling.sh \
 && ./contrib/setup-btor2tools.sh \
 && ./configure.sh --prefix $PREFIX \
 && make PREFIX=$PREFIX -C build -j$(nproc) \
 && make PREFIX=$PREFIX -C build install \
 && cd .. \
 && rm -Rf boolector-$boolector_version

ARG symbiyosys_version="66a458958dc93f8e12418d425e4c31848889937b"
RUN git clone https://github.com/cliffordwolf/SymbiYosys.git SymbiYosys \
 && cd SymbiYosys \
 && git checkout $symbiyosys_version \
 && make PREFIX=$PREFIX -j$(nproc) install \
 && cd .. \
 && rm -Rf SymbiYosys

FROM builder AS build-verilator

ARG DEPS_VERILATOR="perl python3 make autoconf g++ flex bison ccache libgoogle-perftools-dev numactl perl-doc libfl2 libfl-dev zlib1g zlib1g-dev help2man"
RUN apt-get install -y --no-install-recommends $DEPS_VERILATOR

ARG verilator_version="v5.020"
RUN git clone https://github.com/verilator/verilator verilator \
  && cd verilator \
  && git checkout $verilator_version \
  && autoconf \
  && ./configure --prefix $PREFIX \
  && make PREFIX=$PREFIX -j$(nproc) \
  && make PREFIX=$PREFIX install \
  && cd ../.. \
  && rm -Rf verilator

FROM builder AS build-ghc

ARG ghcup_version="0.1.18.0"

# Must be explicitly set
ARG ghc_version
ARG cabal_version

RUN curl "https://downloads.haskell.org/~ghcup/$ghcup_version/x86_64-linux-ghcup-$ghcup_version" --output /usr/bin/ghcup \
 && chmod +x /usr/bin/ghcup \
 && ghcup install ghc $ghc_version --set \
 && ghcup install cabal $cabal_version --set

ARG UBUNTU_VERSION
FROM ubuntu:$UBUNTU_VERSION AS run

LABEL vendor="QBayLogic B.V." maintainer="devops@qbaylogic.com"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH="$PATH:/opt/bin:/root/.ghcup/bin"

ARG DEPS_RUNTIME="ca-certificates ccache curl g++ gcc git jq libc6-dev libgmp10-dev libgnat-10 libllvm13 libreadline8 libtinfo-dev libtcl8.6 make perl python3 ssh zlib1g-dev zstd locales libtinfo5 libx11-6"
RUN apt-get update \
 && apt-get install -y --no-install-recommends $DEPS_RUNTIME \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && locale-gen en_US.UTF-8

COPY --from=build-ghdl /opt /opt
COPY --from=build-iverilog /opt /opt
COPY --from=build-symbiyosys /opt /opt
COPY --from=build-verilator /opt /opt
COPY --from=build-ghc /root/.ghcup /root/.ghcup
