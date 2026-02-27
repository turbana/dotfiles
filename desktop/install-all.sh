#!/bin/bash

set -e

for file in *.desktop; do
    desktop-file-validate $file
    desktop-file-install --dir=$HOME/.local/share/applications $file
done

update-desktop-database ~/.local/share/applications
