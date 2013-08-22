#!/bin/bash

font="-*-fixed-medium-r-*-*-12-90-*-*-*-*-iso8859-1"
#font="Monospace-10"
fg="#cccccc"
bg="#222222"

python ~/.xmonad/cpugraph.py | \
dzen2 -y -1 -x -6000 -w 6000 -ta r -h 20 -fg $fg -bg $bg -fn $font -e 'onstart=lower' -xs 1
