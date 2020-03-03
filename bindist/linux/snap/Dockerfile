# This dockerfile installs nix on /usr/nix (instead of the default /nix/). This
# is used to build snaps of Clash.
FROM ubuntu:18.04

ENV \
  NIX_VERSION=2.3.3 \
  PREFIX=/usr \
  ROOT=/usr/nix \
  STORE=/usr/nix/store \
  VAR=/usr/nix/var \
  BUILD_DEPS='git apt-utils software-properties-common build-essential libbrotli-dev libbrotli1 liblzma-dev curl libcurl4-openssl-dev sqlite libsqlite3-dev pkg-config libcrypto++-dev libcrypto++6 openssl libssl-dev bzip2 libbz2-dev libeditline-dev libeditline0 libreadline-dev libreadline7 libseccomp-dev libgc-dev wget'

COPY edolstra.gpg edolstra.gpg

RUN apt update \
  # Install deps
  && apt install $BUILD_DEPS -y \
  && add-apt-repository ppa:mhier/libboost-latest -y \
  && apt install libboost1.70-dev -y \
  # Download Nix and install it into the system.
  && wget -q https://nixos.org/releases/nix/nix-$NIX_VERSION/nix-$NIX_VERSION.tar.xz \
             https://nixos.org/releases/nix/nix-$NIX_VERSION/nix-$NIX_VERSION.tar.xz.asc \
  && export keyfile=$(mktemp --suffix=.gpg) \
  && gpg --yes -o "$keyfile" --dearmor edolstra.gpg \
  && gpg --batch --no-default-keyring --keyring "$keyfile" --verify nix-$NIX_VERSION.tar.xz.asc nix-$NIX_VERSION.tar.xz \
  && tar xf nix-$NIX_VERSION.tar.xz \
  && addgroup --gid 30000 --system nixbld \
  && for i in $(seq 1 30); do \
       adduser \
         --system \
         --disabled-password \
         --home /var/empty$i \
         --gecos "Nix build user $i" \
         --uid $((30000 + i)) \
         nixbld$i && adduser nixbld$i nixbld; \
     done \
  && mkdir -p -m 0755 $PREFIX/etc/nix \
  && mkdir -m 0755 $ROOT  \
  && cd nix-$NIX_VERSION \
  && EDITLINE_CFLAGS="-DREADLINE" \
     EDITLINE_LIBS="/lib/x86_64-linux-gnu/libhistory.so.7 /lib/x86_64-linux-gnu/libreadline.so.7" \
     ./configure \
       --prefix=$PREFIX \
       --with-store-dir=$STORE \
       --localstatedir=$VAR \
       --with-sandbox-shell=/bin/sh \
       --enable-gc \
  && make \
  && make install \
  && cd .. \
  # Cleanup
  && rm -r nix-$NIX_VERSION nix-$NIX_VERSION.tar.xz \
  && apt autoremove -y \
  && apt clean \
  # Sandboxes not supported in docker containers
  && echo 'sandbox = false' > $PREFIX/etc/nix/nix.conf

# Set environment for next docker image
ONBUILD ENV \
    ENV=$PREFIX/etc/profile.d/nix.sh \
    USER=root
