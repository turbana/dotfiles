#!/bin/bash

#font="-*-fixed-medium-r-*-*-12-*-*-*-*-*-iso8859-*"
#font="Monospace-8"
font="DejaVu Sans Mono-9"
fg="#cccccc"
bg="#222222"

python ~/.xmonad/cpugraph.py | \
dzen2 -y 1060 -x 1420 -w 500 -ta r -sa r -h 20 -fg $fg -bg $bg -fn "$font" -l 16 -e 'button1=togglecollapse'
#dzen2 -y -1 -x -500 -w 500 -ta r -sa r -h 20 -fg $fg -bg $bg -fn $font -xs 1 -l 16 -e 'button1=togglecollapse'
#dzen2 -y -1 -x -6000 -w 6000 -ta r -h 20 -fg $fg -bg $bg -fn $font -e 'onstart=lower' -xs 1 -l 2
