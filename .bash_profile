# Setups system and user profiles and related variables
# /etc/profile will be sourced by bash automatically
# Setups home environment profile
if [ -f ~/.profile ]; then source ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi
# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi


if [ -e /home/zjabbar/.nix-profile/etc/profile.d/nix.sh ]; then . /home/zjabbar/.nix-profile/etc/profile.d/nix.sh; fi

if [[ "$(tty)" == "/dev/tty2" ]]
 then
     sway
fi

export XKB_DEFAULT_OPTIONS=caps:escape
export FONTCONFIG_PATH=/home/zjabbar/.guix-profile/etc/fonts
export PATH=$PATH:/home/zjabbar/.nix-profile/bin

GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
