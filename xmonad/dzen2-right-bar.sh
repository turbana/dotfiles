#!/bin/bash

font="DejaVu Sans Mono-9"
fg=$(getcolor base+3)
bg=$(getcolor base-3)

python ~/.xmonad/cpugraph.py | \
dzen2 -y 1060 -x 1420 -w 500 -ta r -sa r -h 20 -fg $fg -bg $bg -fn "$font" \
	-l 16 -e 'button1=togglecollapse'
