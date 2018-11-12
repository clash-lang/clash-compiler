FROM ubuntu:trusty

ARG PARALLEL=-j8

ENV DEBIAN_FRONTEND noninteractive

ARG DEPS_GHC="pkg-config libgmp-dev"
ARG DEPS_CLASH="libtinfo-dev"
ARG DEPS_CLASH_COSIM="g++-multilib make"
ARG DEPS_IVERILOG_BUILD="autoconf bison flex gperf"

# enable UTF-8 support
RUN locale-gen en_US.UTF-8 && update-locale LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8

# add ghdl & ghc PPA's
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:pgavin/ghdl && \
    add-apt-repository -y ppa:hvr/ghc && \
    apt-get update

RUN apt-get -y install git $DEPS_GHC $DEPS_CLASH $DEPS_CLASH_COSIM ghdl

# needed for iverilog build
RUN apt-get install -y $DEPS_IVERILOG_BUILD
# install iverilog
RUN git clone -b v10-branch --depth 1 https://github.com/steveicarus/iverilog.git iverilog-source && \
    cd iverilog-source && sh autoconf.sh && ./configure && \
    make $PARALLEL && make install && cd .. && rm -r iverilog-source

# Cleanup
RUN apt-get remove -y $DEPS_IVERILOG_BUILD software-properties-common && \
    apt-get autoremove -y && \
    apt-get clean

# keep the apt package cache
RUN rm /etc/apt/apt.conf.d/docker-clean

ARG PREFETCH="cabal-install-head ghc-8.2.2 ghc-8.4.4 ghc-8.6.2"

# Prefetch some packages for speedy installation
RUN apt-get update && apt-get install -y --download-only $PREFETCH

ENV PATH /opt/ghc/bin:/opt/cabal/bin:$PATH
