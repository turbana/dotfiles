#!/bin/bash
# NOTE: this file should be called from Window's Task Scheduler ONCE after boot

set -ex

# setup host specific env
hostrc=~/src/etc/bash/$(hostname)
[ -f $hostrc ] && source $hostrc

# setup gpg-agent for both gpg and ssh keys
# NOTE: copied from ~/src/etc/bash/common so that emacs inherits the right env variables
if [ $(which gpg-connect-agent 2> /dev/null) ]; then
    export GPG_TTY=$(tty)
    gpg-connect-agent updatestartuptty /bye > /dev/null
    export GPG_AGENT_INFO=$(gpgconf --list-dirs agent-socket):0:1
    if [ -n "$(gpgconf --list-options gpg-agent | \
      awk -F: '/^enable-ssh-support:/{ print $1 }')" ]; then
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    fi
fi

/usr/local/bin/emacs --daemon
/usr/bin/nohup /usr/bin/syncthing >~/.config/syncthing/daemon.log 2>&1 &
