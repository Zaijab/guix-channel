#!/bin/sh

source ~/.bash_profile

# No exec: if X dies you get a shell on tty1 instead of a respawn loop.
startx
