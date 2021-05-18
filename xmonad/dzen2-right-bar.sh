#!/usr/bin/env bash

# font="DejaVu Sans Mono-9"
# font="Nimbus Sans L Regular-11"
font="Latin Modern Mono-12"
GC=$HOME/.etc/bin/getcolor
fg=$($GC base+3)
bg=$($GC base-3)

python ~/.xmonad/cpugraph.py | \
dzen2 -y 1060 -x 1420 -w 500 -ta r -sa l -h 20 -fg $fg -bg $bg -fn "$font" \
	-l 16 -e 'button1=togglecollapse' -dock
