#!/usr/bin/env bash
set -euo pipefail

channel="${GUIX_STEAM_CHANNEL:-/home/zjabbar/code/guix-channel/zaijab/files/channel_without_zaijab.tmpl}"

driver_version="$(
    nvidia-smi --query-gpu=driver_version --format=csv,noheader 2>/dev/null \
        | sed -n '1{s/[[:space:]]//g;p}'
)"

if [ -z "$driver_version" ] && [ -r /proc/driver/nvidia/version ]; then
    driver_version="$(
        sed -n 's/.*Kernel Module  \([0-9][0-9.]*\).*/\1/p' \
            /proc/driver/nvidia/version \
            | sed -n '1p'
    )"
fi

if [ -z "$driver_version" ]; then
    echo "Could not determine the loaded Nvidia driver version." >&2
    echo "Is the Nvidia kernel module loaded, and does nvidia-smi work?" >&2
    exit 1
fi

exec guix time-machine -C "$channel" \
    --unsafe-channel-evaluation -- \
    shell "steam-nvidia@$driver_version" -- steam "$@"
