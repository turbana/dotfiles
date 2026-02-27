#!/bin/sh

# delay conky start until the second monitor turns on
sleep 10
# XXX why doesn't this work?
# wait for second display to connect
# while ! (xrandr --query | grep -qP '\bconnected (?!primary)'); do
#     sleep 1
# done

/usr/bin/conky -c conky.config 
