FROM ubuntu:20.04

ARG PARALLEL=-j8

ENV DEBIAN_FRONTEND noninteractive

ARG DEPS_GHC="ghc ghc-doc ghc-prof cabal-debian cabal-install happy"
ARG DEPS_BUILD="build-essential dpkg-dev haskell-devscripts debhelper wget sudo"
ARG DEPS_CLASH="libtinfo-dev libpthread-stubs0-dev"

RUN apt update \
  && apt dist-upgrade -y \
  && apt install $DEPS_GHC $DEPS_BUILD $DEPS_CLASH -y \
  && apt clean \
  && echo 1


# Yeet all possible GHC dependencies into the global scope, so cabal is sure
# to find them. This might not be a good idea.
RUN apt install libghc* -y && apt clean

RUN cabal new-update

#RUN mkdir -p /ghc-and-deps && cd /ghc-and-deps && apt-get download $(apt-cache depends --recurse --no-recommends --no-suggests --no-conflicts --no-breaks --no-replaces --no-enhances ghc ghc-doc ghc-prof cabal-install happy | grep "^\w" | grep -v libc6)
