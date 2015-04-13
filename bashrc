#!/bin/bash

# directory containing dotfiles
export ETC=$HOME/.etc

# include utility functions
if [ -f $ETC/bashrc.util ]; then
	. $ETC/bashrc.util
fi

# quit on non-interactive shell
[ -z "$PS1" ] && return

# enable custom colors
if [ -f ~/.dir_colors ]; then
	eval `dircolors -b ~/.dir_colors`
elif [ -f /etc/DIR_COLORS ]; then
	eval `dircolors -b /etc/DIR_COLORS`
fi

# enable tab completion
[[ -f /etc/bash_completion ]] && . /etc/bash_completion

# update terminal title
case ${TERM} in
	xterm*|rxvt*|Eterm|aterm|kterm|gnome)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
		;;
	screen*)
		PROMPT_COMMAND='echo -ne "\033]0;${PWD/$HOME/~}\033\\"'
		;;
esac

# turn off flow control
stty -ixon

# by default I want files to be 0750.
umask 0027

# append to history, do not overwrite
shopt -s histappend


# some aliases
_LS_OPTS='--color=yes -h --time-style="+%Y-%m-%d %H:%M"'
alias ls="ls $_LS_OPTS"
alias ll='ls -l'
alias la='ls -lA'
alias lad='find . -maxdepth 1 -type d ! -name \. -regex "\./\..*" -print0 | xargs -0 ls -ld '$_LS_OPTS
alias lld='find . -maxdepth 1 -type d ! -name \. ! -regex "\./\..*" -print0 | xargs -0 ls -ld '$_LS_OPTS

# fix my misspellings
alias mc='mv'
alias vp='cp'
alias cd..='cd ..'
alias ci='vi'


# colors used in PS1
e='\[\033'
black="$e[00;30m\\]"
red="$e[00;31m\\]"
green="$e[00;32m\\]"
yellow="$e[00;33m\\]"
blue="$e[00;34m\\]"
magenta="$e[00;35m\\]"
cyan="$e[00;36m\\]"
white="$e[00;37m\\]"

b_black="$e[01;30m\\]"
b_red="$e[01;31m\\]"
b_green="$e[01;32m\\]"
b_yellow="$e[01;33m\\]"
b_blue="$e[01;34m\\]"
b_magenta="$e[01;35m\\]"
b_cyan="$e[01;36m\\]"
b_white="$e[01;37m\\]"

normal="$e[00;00m\\]"


# setup PATH
export PATH=/bin
add_path $HOME/bin
add_path /opt/java/bin
add_path /usr/local/bin
add_path /sbin
add_path /usr/bin
add_path /usr/sbin

export JAVA_HOME=/opt/java

mutt() {
	imap_pid=$HOME/.offlineimap/pid

	if ! ([ -f $imap_pid ] && ps h $(cat $imap_pid) > /dev/null); then
		nohup offlineimap > $HOME/.offlineimap/stdout.log &
		#nohup python $HOME/.offlineimap/code/offlineimap.py > $HOME/.offlineimap/stdout.log &
	fi

	if ps aux | grep $(which mutt) | grep -qv grep; then
		$(which mutt) -R
	else
		$(which mutt)
	fi
}

# setup todo.txt
if which todo.sh > /dev/null; then
	alias t='todo.sh -d $HOME/.todo/config'
	complete -F _todo t
	export TODOTXT_DEFAULT_ACTION=ls
fi

# some general environment variables
export LESS="-SR"
export TERM=xterm-256color

# use emacs as editor
export EDITOR=emacsclient
export ALTERNATE_EDITOR=
alias emacs='emacsclient -c -a ""'

# colorize grep's matches
export GREP_COLOR='1;32'
export GREP_OPTIONS='--color=auto'

if [ -f "${HOME}/.gpg-agent-info" ]; then
	. "${HOME}/.gpg-agent-info"
	export GPG_AGENT_INFO
	export SSH_AUTH_SOCK
fi

if at_work && [ -f $ETC/bashrc.work ]; then
	. $ETC/bashrc.work
elif at_home && [ -f $ETC/bashrc.home ]; then
	. $ETC/bashrc.home
fi

export PS1="${ps1_user}\u${normal}@${ps1_host}\h ${cyan}\w ${b_red}\$(_prompt_fail)${normal}\$ "
