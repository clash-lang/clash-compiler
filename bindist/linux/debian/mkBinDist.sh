#!/bin/bash
set -e

apt update
apt dist-upgrade -y
apt install libghc-* -y

HERE=$(dirname $0)
ROOT=${HERE}/../../..

cd ${ROOT}
# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done
rm -rf dist-newstyle
cabal new-build clash-ghc --dry-run
cd ${HERE}

python3 -B build_debs.py

exit

# Fetch all deb dependencies
mkdir -p ghc-and-deps
cd ghc-and-deps
if [[ -d /ghc-and-deps ]]; then
  mv /ghc-and-deps/*.deb .
fi
apt-get download $(apt-cache depends --recurse --no-recommends --no-suggests --no-conflicts --no-breaks --no-replaces --no-enhances ghc ghc-doc ghc-prof cabal-install happy | grep "^\w" | grep -v libc6)
cd ${HERE}

# Extract all debs and tarball them
mkdir -p dist
cd dist
echo " " >> ../debs
ls ../ghc-and-deps/*.deb | tr '\n' ' ' >> ../debs
for deb in $(cat ../debs); do
  ar x ${deb}
  tar xf data.tar.xz
done
XZ_OPT="-T $(nproc) -1" tar -cJf ../debdist.tar.xz $(ls -d */)
cd ..

# Cleanup
rm debs
rm -rf dist
