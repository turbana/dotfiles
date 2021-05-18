# ~/.profile

# source bashrc if it exists
[ -f $HOME/.bashrc ] && source $HOME/.bashrc

# check for auto starting guix profiles
GUIX_AUTO_PROFILES=~/.config/guix/auto-start-profiles
for profile in $GUIX_AUTO_PROFILES/*; do
    if [ -f "${profile}/etc/profile" ]; then
        GUIX_PROFILE=$profile ; source ${profile}/etc/profile
    fi
    unset profile
done
