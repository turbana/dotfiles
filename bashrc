#!/bin/bash

# directory containing dotfiles
export ETC=$HOME/.etc

# add to the path, but only if it's not already there
add_path() {
	# NOTE: because we are setting $PATH, we shouldn't rely on it
	[ $# -eq 1 ] || return 2
	[ -d "$1" ] || return 1
	if echo $PATH | /bin/grep -Evq "(^|:)${1}(:|$)"; then
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
