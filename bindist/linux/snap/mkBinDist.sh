#!/usr/bin/env bash
set -e
set -x

# Path to thhe nix file containing the derivation we want to package
NIXFILE=$(dirname $0)/bindist.nix

# "drv" will be set to the path inside /nix/store which contains the resulting
# the derivation
drv=$(nix-build $NIXFILE)

# output filename
tarball=$(pwd)/clash-snap-bindist.tar.xz

# temporary working directory
tmpdir=$(mktemp -d)
trap "rm -rf $tmpdir" EXIT
cd $tmpdir

# create symlinks inside target's /bin to every executable in the derivation
# (non-transitively)
mkdir -p bin
for binary in $(find ${drv}/bin); do
  ln -s ${binary} bin/$(basename ${binary})
done

rm bin/bin

# Copy nix derivation, including fully qualified directory name, to this
# folder. There's probably a better way to do this?
tar cf - --owner 0 --group 0 $(nix-store -qR $drv) | tar xf -

# Allow symlinks to be deleted
find * -type d -exec chmod +w {} \;

# Convert absolute symlinks to relative symlinks (needed for snap)
find . -lname '/*' -exec ksh -c '
  for link; do
    target=$(readlink "$link")
    link=${link#./}
    root=${link//+([!\/])/..}; root=${root#/}; root=${root%..}
    rm "$link"
    ln -s "$root${target#/}" "$link"
  done
' _ {} +

# Package it for snapcraft. Snapcraft will later recompress it more
# thorougly. Not using /no compression/ as these files will be used
# in GitLab artifacts.
XZ_DEFAULT="-T 0 -3" tar cJf $tarball --owner 0 --group 0 *
