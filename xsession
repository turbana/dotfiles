#!/bin/bash

. ~/.etc/bashrc.util


# needed for border repainting with xmonad and virtualbox
xcompmgr &

feh --bg-fill $HOME/.xmonad/black.png

gnome-terminal &

if at-work; then
	pidgin &
	icedove &
fi

gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"
offlineimap &

# xmonad config needs our hostname in $HOST
HOST=$(hostname) xmonad
