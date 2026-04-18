#!/usr/bin/env bash
# Launch Google Chrome with cgroup resource limits.
# Memory: 4G hard cap | CPU: 1.5 cores max

CGROUP_PATH="/sys/fs/cgroup/chrome"
MEMORY_MAX="4G"
CPU_MAX="150000 100000"

# Create and configure the cgroup if needed
if [ ! -d "$CGROUP_PATH" ]; then
    sudo mkdir -p "$CGROUP_PATH"
    echo "+memory +cpu" | sudo tee /sys/fs/cgroup/cgroup.subtree_control > /dev/null
fi

echo "$MEMORY_MAX" | sudo tee "$CGROUP_PATH/memory.max" > /dev/null
echo "$CPU_MAX" | sudo tee "$CGROUP_PATH/cpu.max" > /dev/null

# Move this shell into the cgroup so chrome and all its children inherit it
echo $$ | sudo tee "$CGROUP_PATH/cgroup.procs" > /dev/null

exec guix time-machine -C /home/zjabbar/code/guix-channel/zaijab/files/channel_without_zaijab.tmpl \
     -- shell google-chrome-stable -- google-chrome "$@"
