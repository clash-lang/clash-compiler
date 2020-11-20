#!/usr/bin/env bash
set -e

export XZ_DEFAULTS="${XZ_DEFAULTS:-"-T 0 -3"}"
export THREADS="${THREADS:-$(nproc)}"

# Path to the nix file containing the derivation we want to package
NIXFILE=$(dirname $0)/bindist.nix

# "drv" will be set to the path inside /nix/store which contains the resulting the derivation
drv=$(nix-build $NIXFILE -j${THREADS})

# output filename
tarball=$(pwd)/clash-bindist.tar.xz

# temporary working directory
tmpdir=$(mktemp -d)
trap "rm -rf $tmpdir" EXIT

cp $(dirname $0)/wrappers/clash $tmpdir/
cp $(dirname $0)/wrappers/clashi $tmpdir/

cd $tmpdir

# create symlinks inside target's ./bin to every executable in the derivation (non-transitively)
mkdir -p bin
for binary in $(find ${drv}/bin); do
  ln -s ${binary} bin/$(basename ${binary})
done
# Remove spurious copy of the bin directory
rm -rf ./bin/bin

# Replace nix-user-chroot symlink with actual binary
user_chroot=$(readlink -f ./bin/nix-user-chroot)
rm ./bin/nix-user-chroot
mkdir nix-user-chroot
cp $user_chroot ./nix-user-chroot/

# finally, pack the full closure of the derivation with the symlinks
# we just created
tar cJf $tarball --owner 0 --group 0 bin clash clashi nix-user-chroot $(nix-store -qR $drv)
