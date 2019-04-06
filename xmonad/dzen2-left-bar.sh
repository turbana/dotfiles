#!/bin/bash

font="DejaVu Sans Mono-9"
GC=$HOME/.etc/bin/getcolor
fg=$($GC base+3)
bg=$($GC base-3)
bar=$($GC base-2)

# I don't see a way to configure what Xmonad is sending us without
# re-implementing an entire logger. I don't want to see what layout mode we're
# in so strip everything between the two | with sed
sed -ue 's/|[^|]*|/|/' | \
# start with a space
sed -ue 's/^/ /' | \
# insert our bar highlight
sed -ue 's/$/^pa(0)^fg('"$bar"')^ib(1)^ro(1420x1-0-9)^ib(0)/' | \
dzen2 -y 1060 -x 0 -w 1420 -ta l -h 20 -fg $fg -bg $bg -fn "$font" -xs 1 -dock
