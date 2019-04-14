#!/bin/bash

_prompt_fail() {
    ec=$?
    if [ $ec -ne 0 ]; then
		echo "<$ec> "
    fi
	true
}
export PS1="${ps1_user}\u${normal}@${ps1_host}\h ${cyan}\w ${b_red}\$(_prompt_fail)${normal}\$ "