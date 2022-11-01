#!/bin/bash

link() {
	src=$1
	dst=$2
	if [ ! $src -ef $dst ]; then
		if [ -e $dst ]; then
			mv $dst ${dst}.bak
		fi
		ln -sf $(pwd)/$src $dst
	fi
}

dir() {
	path=$1
	if [ ! -d $path ]; then
		if [ -e $path ]; then
			mv $path ${path}.bak
		fi
		mkdir -p $path
	fi
}

link_recursive() {
	src_path=$1
	dst_path=$2
	dir $dst_path
	for file in $(cd $src_path; find . | cut -b3-); do
		sfile=$src_path/$file
		dfile=$dst_path/$file
		if [ -f $sfile ]; then
			link $sfile $dfile
		elif [ -d $sfile ]; then
			dir $dfile
		fi
	done

}

cd $(dirname $0)

link bash_logout	~/.bash_logout
link bashrc			~/.bashrc
link dir_colors		~/.dir_colors
link profile		~/.profile
link screenrc		~/.screenrc
#link vimrc			~/.vimrc
#link xsession		~/.xsession
link xresources		~/.Xresources

#link_recursive mutt		~/.mutt
#link_recursive vim		~/.vim
#link_recursive xmonad	~/.xmonad
