#!/bin/bash
set -euo pipefail

SNAPCRAFT_BUILD_ENVIRONMENT_CPU=$(nproc)
export SNAPCRAFT_BUILD_ENVIRONMENT_CPU
SNAPCRAFT_BUILD_ENVIRONMENT_MEMORY=16G
export SNAPCRAFT_BUILD_ENVIRONMENT_MEMORY

# Prevent running if we've already published this revision
apt update
apt install git -y
touch snap-last-run-hash
if [ "$(cat snap-last-run-hash)" == "$(git rev-parse HEAD)-${RELEASE_CHANNEL}" ]; then
  echo "Already built and published $(git rev-parse HEAD) on ${RELEASE_CHANNEL}. Nothing to do!";
  exit 0;
fi
git rev-parse HEAD > snap-last-run-hash
echo "-${RELEASE_CHANNEL}" >> snap-last-run-hash

cd bindist/linux/snap || exit

# Make sure devel is in snap/snapcraft.yaml before replacing it with 'stable'
# (if applicable). sed doesn't fail if it doesn't replace anything.
grep devel snap/snapcraft.yaml
if [[ ${RELEASE_CHANNEL} == "stable" || ${RELEASE_CHANNEL} == "beta" ]]; then
  # The Snap Store only allows grade=stable for stable snaps
  sed -i s/devel/stable/ snap/snapcraft.yaml
fi

echo $SNAPCRAFT_LOGIN_FILE | base64 --decode --ignore-garbage > snapcraft.login
snapcraft login --with snapcraft.login
snapcraft
snapcraft push ./*.snap --release ${RELEASE_CHANNEL}
