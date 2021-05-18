#!/usr/bin/env bash

SOURCE_DIRS=".etc as src .mail"
#SOURCE_DIRS=".etc as"
BACKUP_DIR="/home/iclark/host/share/backups"

TARFILE="$BACKUP_DIR/backup-$(date +%F_%H%M).tar.bz2"
TARARGS="--exclude src/ewu/svn"

cd ~
tar -cjf $TARFILE $TARARGS $SOURCE_DIRS
