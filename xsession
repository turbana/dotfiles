#!/bin/bash

. ~/.etc/bashrc.util
export PATH="$HOME/.cabal/bin:$PATH"

if at_work; then
	#VBoxManage setextradata global CustomVideoMode1 1050x1680
	xrandr --newmode 1050x1680 197.06 1048 1080 1824 1856 1680 1714 1730 1765
	xrandr --addmode VBOX1 1050x1680
	xrandr --addmode VBOX2 1050x1680
	xrandr --output VBOX0 --auto --primary
	xrandr --output VBOX1 --auto --left-of  VBOX0 --mode 1050x1680
	xrandr --output VBOX2 --auto --right-of VBOX0 --mode 1050x1680

	xsetroot -cursor_name left_ptr
fi

# needed for border repainting with xmonad and virtualbox
xcompmgr &
feh --bg-fill $HOME/.xmonad/black.png

emacs &
gnome-terminal &

if at_work; then
	pidgin &
fi

gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"

# xmonad config needs our hostname in $HOST
HOST=$(hostname) exec xmonad
