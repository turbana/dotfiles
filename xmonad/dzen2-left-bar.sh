#!/bin/bash

#font="-*-fixed-medium-r-*-*-12-*-*-*-*-*-iso8859-*"
font="Monospace-10"
fg="#ffffff"
bg="#222222"

# the right bar covers the entire screen so wait a bit to ensure we paint on top
sleep 1

# I don't see a way to configure what Xmonad is sending us without reimplementing
# an enitre logger. I don't want to see what layout mode we're in so strip everything
# between the two | with sed
sed -ue 's/|[^|]*|/|/' | \
# start with a space
sed -ue 's/^/ /' | \
dzen2 -y -1 -x 0 -w 600 -ta l -fg $fg -bg $bg -fn $font 
