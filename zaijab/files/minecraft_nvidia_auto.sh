#!/usr/bin/env bash
set -euo pipefail

channel="${GUIX_MINECRAFT_CHANNEL:-/home/zjabbar/code/guix-channel/zaijab/files/minecraft.tmpl}"

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

driver_major="${driver_version%%.*}"

cores="${GUIX_MINECRAFT_CORES:-2}"

exec guix time-machine -C "$channel" \
    --unsafe-channel-evaluation -- \
    shell prismlauncher jbr@21 "--with-graft=mesa=nvda@$driver_major" "--cores=$cores" -- \
    prismlauncher "$@"
