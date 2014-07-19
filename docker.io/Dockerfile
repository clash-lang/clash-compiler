# Use phusion/baseimage as base image. To make your builds
# reproducible, make sure you lock down to a specific version, not
# to `latest`! See
# https://github.com/phusion/baseimage-docker/blob/master/Changelog.md
# for a list of version numbers.
FROM phusion/baseimage:0.9.11 

# Set correct environment variables.
ENV HOME /root
ENV LANG en_US.UTF-8

# Regenerate SSH host keys. baseimage-docker does not contain any, so you
# have to do that yourself. You may also comment out this instruction; the
# init system will auto-generate one during boot.
#RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

# Install development tools
RUN apt-get update -y
RUN apt-get install -y git build-essential make gcc libgmp3-dev zlib1g-dev libncurses5-dev wget

# Install GHC 7.8
RUN mkdir -p /opt/ghc
WORKDIR /tmp
RUN wget http://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-x86_64-unknown-linux-deb7.tar.bz2
RUN tar -xjvf ghc-7.8.2-x86_64-unknown-linux-deb7.tar.bz2
WORKDIR /tmp/ghc-7.8.2
RUN ./configure prefix=/opt/ghc
RUN make install
ENV PATH $PATH:/opt/ghc/bin

# Install Cabal
WORKDIR /tmp
RUN wget http://www.haskell.org/cabal/release/cabal-install-1.20.0.2/cabal-install-1.20.0.2.tar.gz 
RUN tar -xzvf cabal-install-1.20.0.2.tar.gz 
WORKDIR /tmp/cabal-install-1.20.0.2
RUN ./bootstrap.sh
ENV PATH $PATH:$HOME/.cabal/bin
WORKDIR /
RUN cabal update

# Install Clash
RUN cabal install clash-ghc

# Use baseimage-docker's init system
CMD ["/sbin/my_init"]

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
