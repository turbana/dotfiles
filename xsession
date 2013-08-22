#!/bin/bash

. ~/.etc/bashrc.util

if at_work; then
	xrandr --output VBOX0 --auto --primary
	xrandr --output VBOX1 --auto --left-of VBOX0 --mode 1024x1280
fi

# needed for border repainting with xmonad and virtualbox
xcompmgr &
feh --bg-fill $HOME/.xmonad/black.png

gnome-terminal &

if at_work; then
	pidgin &
fi

gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"

# xmonad config needs our hostname in $HOST
HOST=$(hostname) exec xmonad
