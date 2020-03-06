#!/bin/bash
set -euo pipefail

SNAPCRAFT_BUILD_ENVIRONMENT_CPU=$(nproc)
export SNAPCRAFT_BUILD_ENVIRONMENT_CPU
SNAPCRAFT_BUILD_ENVIRONMENT_MEMORY=16G
export SNAPCRAFT_BUILD_ENVIRONMENT_MEMORY

# Install git to detect branch names / revisions
apt update
apt install git -y

# Set release channel based on branch:
#
# A build with RELEASE_CHANNEL=beta_or_edge only runs at night as a schedule
# (or if a schedule is manually triggered). The schedules target either
# master or a release branch (1.0, 1.2, ..). If it's run on master it should
# go into Snap's 'edge' channel, otherwise it's a pre-release dot-version
# (i.e., latest released version + fixes accumulated in release branch) thus
# beta.
branch_name=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD)
if [[ ${branch_name} == "master" && ${RELEASE_CHANNEL} == "beta_or_edge" ]]; then
  RELEASE_CHANNEL="edge"
else
  RELEASE_CHANNEL="beta"
fi

# Prevent running if we've already published this revision
hash_file=snap-last-run-hash
revision=$(git rev-parse HEAD)

touch ${hash_file}
if [ "$(cat ${hash_file})" == "${revision}" ]; then
  echo "Already built and published ${revision} on ${RELEASE_CHANNEL}. Nothing to do!";
  exit 0;
fi
echo "${revision}" > ${hash_file}

cd bindist/linux/snap || exit

# Make sure devel is in snap/snapcraft.yaml before replacing it with 'stable'
# (if applicable). sed doesn't fail if it doesn't replace anything.
grep devel snap/snapcraft.yaml
if [[ ${RELEASE_CHANNEL} == "stable" || ${RELEASE_CHANNEL} == "beta" ]]; then
  # The Snap Store only allows grade=stable for stable snaps
  sed -i s/devel/stable/ snap/snapcraft.yaml
fi

echo "$SNAPCRAFT_LOGIN_FILE" | base64 --decode --ignore-garbage > snapcraft.login
snapcraft login --with snapcraft.login
snapcraft
snapcraft push ./*.snap --release ${RELEASE_CHANNEL}
