#!/usr/bin/env bash

source $HOME/.bashrc

# needed for xmonad
add_path $HOME/.cabal/bin

# assume we want to run a WM
USE_WINDOW_MANAGER=1

if at_work; then
	#VBoxManage setextradata global CustomVideoMode1 1050x1680
	xrandr --newmode 1050x1680 197.06 1048 1080 1824 1856 1680 1714 1730 1765
	xrandr --addmode VBOX1 1050x1680
	xrandr --addmode VBOX2 1050x1680
	xrandr --output VBOX0 --auto --primary
	xrandr --output VBOX1 --auto --left-of  VBOX0 --mode 1050x1680
	xrandr --output VBOX2 --auto --right-of VBOX0 --mode 1050x1680

	xsetroot -cursor_name left_ptr
    
elif on_host "cyclone"; then
	xrandr --newmode "1920x1080_vbox"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
	xrandr --addmode Virtual2 1920x1080_vbox
	xrandr --output Virtual2 --mode 1920x1080_vbox --right-of Virtual1
    
elif on_host "tornado"; then
	xrandr --newmode "1920x1080_vbox"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
	xrandr --addmode Virtual-1 1920x1080_vbox
	xrandr --addmode Virtual-2 1920x1080_vbox
	xrandr --output Virtual-1 --mode 1920x1080_vbox
	xrandr --output Virtual-2 --mode 1920x1080_vbox --right-of Virtual-1

    # start gnu herd
    shepherd

elif on_host "Fanghorn"; then
    # Fanghorn is a Windows machine running WSL
    USE_WINDOW_MANAGER=0
    nohup syncthing > ~/tmp/syncthing.log &
    nohup emacsclient -c -e '(ic-open-org-agenda)' -a '' > ~/tmp/emacs.log &
fi

xrdb -merge $HOME/.Xresources

if [[ "$USE_WINDOW_MANAGER" == "1" ]]; then
    unset USE_WINDOW_MANAGER
    # xmonad config needs our hostname in $HOST
    HOST=$(hostname) exec xmonad
fi
