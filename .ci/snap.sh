#!/bin/bash
set -euo pipefail
set -x

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
branch_name=${CI_COMMIT_REF_SLUG}
if [[ ${RELEASE_CHANNEL} == "beta_or_edge" ]]; then
  if [[ ${branch_name} == "master" ]]; then
    RELEASE_CHANNEL="edge"
  else
    RELEASE_CHANNEL="beta"
  fi
fi

# Prevent running if we've already published this revision
hash_file=snap-last-run-hash
revision=$(git rev-parse HEAD)

touch ${hash_file}
if [ "$(cat ${hash_file})" == "${revision}" ]; then
  echo "Already built and published ${revision} on ${RELEASE_CHANNEL}. Nothing to do!";
  exit 0;
fi

cd .ci/bindist/linux/snap || exit

if [[ ${RELEASE_CHANNEL} == "stable" || ${RELEASE_CHANNEL} == "beta" ]]; then
  # The Snap Store only allows grade=stable for stable snaps
  sed -i s/devel/stable/ snap/snapcraft.yaml
fi

set +x

if [[ $1 == "build" ]]; then
  ./go.sh
fi

if [[ $1 == "publish" ]]; then
  echo "$SNAPCRAFT_LOGIN_FILE" | base64 --decode --ignore-garbage > snapcraft.login
  snapcraft login --with snapcraft.login
  snapcraft upload ./*.snap --release ${RELEASE_CHANNEL}
  echo "${revision}" > ${hash_file}
fi
