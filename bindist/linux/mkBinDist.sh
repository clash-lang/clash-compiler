#!/usr/bin/env bash
set -e

# Path to thhe nix file containing the derivation we want to package
NIXFILE=$(dirname $0)/bindist.nix

# "drv" will be set to the path inside /nix/store which contains the resulting the derivation
drv=$(nix-build $NIXFILE)

# output filename
tarball=$(pwd)/clash-bindist.tar.xz

# temporary working directory
tmpdir=$(mktemp -d)
trap "rm -rf $tmpdir" EXIT

g++ -o $tmpdir/nix-user-chroot -DENV_PATH='"/bin"' $(dirname $0)/nix-user-chroot/main.cpp
cp $(dirname $0)/wrappers/clash $tmpdir/
cp $(dirname $0)/wrappers/clashi $tmpdir/

cd $tmpdir

# create symlinks inside target's usr/local/bin to every executable in the derivation (non-transitively)
mkdir -p bin
for binary in $(find ${drv}/bin); do
  ln -s ${binary} bin/$(basename ${binary})
done

# finally, pack the full closure of the derivation with the symlinks
# we just created
tar cJf $tarball --owner 0 --group 0 bin clash clashi nix-user-chroot $(nix-store -qR $drv)
