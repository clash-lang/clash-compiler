FROM ubuntu:groovy

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update \
 && apt-get install -y \
      devscripts cabal-install cabal-debian haskell-devscripts \
      ghc ghc-prof ghc-doc python3 \
 && cabal update
