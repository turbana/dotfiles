#!/bin/bash

source $HOME/.bashrc

# needed for xmonad
add_path $HOME/.cabal/bin

if at_work; then
	#VBoxManage setextradata global CustomVideoMode1 1050x1680
	xrandr --newmode 1050x1680 197.06 1048 1080 1824 1856 1680 1714 1730 1765
	xrandr --addmode VBOX1 1050x1680
	xrandr --addmode VBOX2 1050x1680
	xrandr --output VBOX0 --auto --primary
	xrandr --output VBOX1 --auto --left-of  VBOX0 --mode 1050x1680
	xrandr --output VBOX2 --auto --right-of VBOX0 --mode 1050x1680

	xsetroot -cursor_name left_ptr
else
    xrandr --dpi 94
fi

emacs &
gnome-terminal &

# xmonad config needs our hostname in $HOST
HOST=$(hostname) exec xmonad
