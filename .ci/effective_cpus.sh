#!/bin/bash
# `nproc` doesn't account for limits set by cgroups/docker. This script tries
# to determine the effective number of cpus we can use by inspecting the shares
# it has been given.
set -euo pipefail
IFS=$'\n\t'

if [ -f /sys/fs/cgroup/cpu/cpu.cfs_quota_us ]; then
  # Older kernels (<= Ubuntu 20.04)
  cfs_quota_us=$(cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us)
  cfs_period_us=$(cat /sys/fs/cgroup/cpu/cpu.cfs_period_us)
elif [ -f /sys/fs/cgroup/cpu.max ]; then
  # Newer kernels (>= Ubuntu 22.04)
  cfs_quota_us=$(cat /sys/fs/cgroup/cpu.max | awk '{ print $1 }')
  cfs_period_us=$(cat /sys/fs/cgroup/cpu.max | awk '{ print $2 }')
else
  echo "Could not determine number of effective CPUs"
  exit 1
fi

if [[ ${cfs_quota_us} == -1 ]]; then
  # No limits set
  nproc
else
  expr "${cfs_quota_us}" / "${cfs_period_us}"
fi

