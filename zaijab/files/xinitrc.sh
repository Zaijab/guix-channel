#!/bin/sh

source ~/.bash_profile

DIR=$HOME/.guix-home/profile

$DIR/bin/xinit -- $DIR/bin/Xorg :0 vt1 -keeptty \
                -configdir $DIR/share/X11/xorg.conf.d \
                -modulepath $DIR/lib/xorg/modules
