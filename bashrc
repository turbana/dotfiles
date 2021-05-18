#!/usr/bin/env bash

# directory containing dotfiles
export ETC=$(dirname $(readlink -f ~/.bashrc))

# keep grep so 'add_path' can work without $PATH
export _GREP=$(which grep)

# add to the path, but only if it's not already there
add_path() {
	# NOTE: because we are setting $PATH, we shouldn't rely on it
	[ $# -eq 1 ] || return 2
	[ -d "$1" ] || return 1
	if echo $PATH | $_GREP -Evq "(^|:)${1}(:|$)"; then
		if [ -z "$PATH" ]; then
			export PATH=$1
		else
			export PATH=$1:$PATH
		fi
	fi
}

# check if $(hostname) is found in file ($1)
host_match() {
	[ $# -eq 1 ] || return 2
	[ -f $1 ] || return 1
	grep -q "$(hostname)" $1
}

# are we work?
at_work() {
	host_match $ETC/bash/hostnames.work
}

# are we at home?
at_home() {
	host_match $ETC/bash/hostnames.home
}

# are we running in a guix system?
on_guix() {
    # HACK just check for an /etc/guix directory
    test -d /etc/guix
}

# source a file only if it exists
try_source() {
	[ $# -eq 1 ] || return 2
	[ -f $1 ] || return 1
	source $1
}


# source config files
# NOTE: source ps1 later so scripts can set ${ps1_user} and ${ps1_host}
try_source $ETC/bash/common
try_source $ETC/bash/interactive
[ at_home ] && try_source $ETC/bash/home
[ at_work ] && try_source $ETC/bash/work
try_source $ETC/bash/$(hostname)
try_source $ETC/bash/interactive.ps1

unset _GREP
