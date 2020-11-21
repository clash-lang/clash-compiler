#!/usr/bin/env bash
export CPATH=$SNAP/usr/include:$SNAP/usr/include/x86_64-linux-gnu
$SNAP/usr/bin/clashi $@
