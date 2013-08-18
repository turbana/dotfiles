#!/bin/sh

. $ETC/bashrc.util

if at_work; then
	$ETC/mutt/maybe-source.sh $HOME/.mutt/muttrc.work
elif at_home; then
	$ETC/mutt/maybe-source.sh $HOME/.mutt/muttrc.home
else
	echo /dev/null
fi
