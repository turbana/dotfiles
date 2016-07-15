#!/bin/bash

font="-*-fixed-medium-r-*-*-12-*-*-*-*-*-iso8859-*"
#font="Monospace-8"
fg="#ffffff"
bg="#222222"

# I don't see a way to configure what Xmonad is sending us without reimplementing
# an entire logger. I don't want to see what layout mode we're in so strip everything
# between the two | with sed
sed -ue 's/|[^|]*|/|/' | \
# start with a space
sed -ue 's/^/ /' | \
dzen2 -y 1060 -x 0 -w 1420 -ta l -h 20 -fg $fg -bg $bg -fn "$font" -xs 1 -e 'onstart=lower'
