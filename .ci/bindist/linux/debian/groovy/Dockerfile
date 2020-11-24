FROM ubuntu:groovy

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update \
 && apt-get install -y \
      devscripts cabal-install cabal-debian haskell-devscripts haskell-stack \
      ghc ghc-prof ghc-doc python3 equivs \
      libghc-aeson-dev libghc-aeson-doc libghc-aeson-prof \
      libghc-lens-dev libghc-lens-doc libghc-lens-prof \
 && cabal update
