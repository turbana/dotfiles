# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# startx if haven't yet
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
