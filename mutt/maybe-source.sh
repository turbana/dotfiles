#!/bin/sh

if [ -f $1 ]; then
	echo $1
else
	echo /dev/null
fi
