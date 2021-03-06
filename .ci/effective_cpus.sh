#!/bin/bash
# `nproc` doesn't account for limits set by cgroups/docker. This script tries
# to determine the effective number of cpus we can use by inspecting the shares
# it has been given.
set -euo pipefail
IFS=$'\n\t'

cfs_quota_us=$(cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us)
cfs_period_us=$(cat /sys/fs/cgroup/cpu/cpu.cfs_period_us)

if [[ ${cfs_quota_us} == -1 ]]; then
  # No limits set
  nproc
else
  expr "${cfs_quota_us}" / "${cfs_period_us}"
fi
