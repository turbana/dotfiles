" If vim was compiled with syntax and our terminal supports color, enable color
if has('syntax') && (&t_Co > 2)
	syntax on
	set t_Co=256
	color ian
endif

set number                      " Display line numbers
set nocompatible				" Use Vim defaults not Vi
set autoindent					" Turn on autoindenting
set linebreak					" Don't wrap words by default
"set textwidth=80				" Lines should only be 80 characters
set history=50					" Keep 50 lines of command line history
set ruler						" Show the cursor position all the time
set backspace=indent,eol,start	" More powerful backspacing
set ignorecase					" Do case insensitive matching ...
set smartcase					" ... unless they contain uppercase characters
set incsearch					" Move to matches as we are searching
set shiftround
set nowrap						" Don't wrap long lines
set noexpandtab					" Don't replace tabs with spaces
set nohlsearch					" Don't highlight all matches for a search
set tabstop=4					" Tabstops are 4
set shiftwidth=4		
set smarttab					" If tabs get set to spaces, backspace over
                                " shiftwidth amount of spaces
set autoindent					" Automatically indent new lines
set wildmode=list:longest,full	" Better tab-complete on command prompt
set scrolloff=5					" Scroll when cursor is +/- 5 lines from edge
set visualbell					" No beep for you!
set undolevels=200				" Ensure 200 levels of undo
set showcmd						" Show the last command in the bottom right.
                                " (shows amount highlighted in visual mode)
set magic						" Use extended regular expressions
set t_ut=

" By default '<reg> moves to the line that <reg> is marked at and `<reg> moves
" to the line and column. Swap those keys
nnoremap ' `
nnoremap ` '

" Ignore the following wildcards when completing file/directoy names
set wildignore=*.o,*~

set nojoinspaces				" Do not place multiple spaces after a . when joining

" Turn on filetype specific files
filetype plugin indent on

let mapleader=","

" Keep backups/swapfiles/infofile in one location to remove cluter
set backup
set backupdir=~/.vim/backups,.
set directory=~/.vim/swap//,.,~/tmp,/var/tmp,/tmp
set viminfo='20,\"50,n~/.vim/viminfo.dat

" Restore cursor to last known position when reopening a file
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif

" Setup file explorer mappings
map <Leader>e :Vexplore .<CR>
map <Leader>E :Vexplore ~/<CR>
" Hide certain files
let g:netrw_list_hide='^\.[^.],\.pyc$,\.o$,\.gz$,\.bz$'
" Display long listing with date/time formatted for 24H
let g:netrw_timefmt="%a %d %b %Y %T"
let g:netrw_liststyle=1

" Make :help split the window horizontally
autocmd FileType help wincmd L


set tags=./tags;../../../../

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Macros """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Shift-C will remove leading whitespace from a selection
vmap <S-C> :s/\v^[ \t]*//<CR>

" Shift-Insert will insert like a middle mouse button
"map <S-Insert> <MiddleMouse>
"imap <S-Insert> <MiddleMouse>

" Allow * and # to work in Visual Mode
vnoremap * y/\V<C-R>=substitute(escape(@@,"/\\"),"\n","\\\\n","ge")<CR><CR>
vnoremap # y?\V<C-R>=substitute(escape(@@,"?\\"),"\n","\\\\n","ge")<CR><CR>

" Make Q reformat paragraph, comment section, code section, etc
map Q gqap

" Allow ,y(ank) and ,p(aste) to work between sessions
vmap <Leader>y y:new<cr>:call setline(1,getregtype())<cr>o<esc>P:wq! ~/.vim/reg.dat<cr>
nmap <Leader>y :new<cr>:call setline(1,getregtype())<cr>o<esc>P:wq! ~/.vim/reg.dat<cr>
map  <Leader>p :sview ~/.vim/reg.dat<cr>"zdddG:q!<cr>:call setreg('"', @", @z)<cr>p
map  <Leader>P :sview ~/.vim/reg.dat<cr>"zdddG:q!<cr>:call setreg('"', @", @z)<cr>P

" Pipe lines through bc and write back output
nmap gbc :.!bc<CR>
vmap gbc :!bc<CR>

" Pipe lines through python and write back output
nmap gpy :.!python<CR>
vmap gpy :!python<CR>




autocmd BufEnter * let &titlestring = expand("%:t")
"autocmd BufLeave * let &titlestring = "foo"
"autocmd BufLeave * let &titleold = "foo2"
let &titleold = "bash"

if &term =~ "^screen"
	set t_ts=k
	set t_fs=\
endif

if &term =~ "^screen" || &term =~ "^xterm"
	set title
endif


map <F9> :make<CR>

function ShowErrors()
"	if &textwidth > 0
"		exec 'match ErrorMsg /\%>' . &textwidth . 'v.\+/'
"	endif
endfunction

function SetupSpellCheck()
	map <F5> <ESC>:w<CR>:!aspell -c --dont-backup "%"<CR>:e! "%"<CR><CR>
endfunction






""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Custom file types """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function FiletypePython()
	set makeprg=python\ $*\ %
"	set errorformat=%E\ \ File\ \"%f\"\\,\ line\ %l\\,\ column\ %c,%C%m
"setlocal errorformat=
"	\%A\ \ File\ \"%f\"\\\,\ line\ %l\\\,%m,
"	\%C\ \ \ \ %.%#,
"	\%+Z%.%#Error\:\ %.%#,
"	\%A\ \ File\ \"%f\"\\\,\ line\ %l,
"	\%+C\ \ %.%#,
"	\%-C%p^,
"	\%Z%m,
"	\%-G%.%#

	" keep the interpreter open when executing
	map <S-F9> :make -i<CR>
	imap <S-F9> <ESC>:make -i<CR>
endfunction


function FiletypeRedcode()
	let redcode_highlight_numbers=1
	set tabstop=8
	set shiftwidth=8
	set makeprg=pmarsx\ -e\ %
endfunction


function FiletypeHaskell()
	set textwidth=0
	set tabstop=2
	set shiftwidth=2
	set expandtab
endfunction


function FiletypeSQL()
	set textwidth=0
	set tabstop=2
	set shiftwidth=2
	set expandtab
endfunction


function FiletypeText()
	set textwidth=72
	set expandtab
	call SetupSpellCheck()
endfunction


function FiletypeC()
	" Move to the corresponding source/header file
	nmap <Leader>S :find %:t:r.c<cr>
	nmap <Leader>s :sf %:t:r.c<cr>
	nmap <Leader>H :find %:t:r.h<cr>
	nmap <Leader>h :sf %:t:r.h<cr> 
endfunction


function FiletypeTex()
	set tabstop=2
	set shiftwidth=2
	set expandtab
	set textwidth=72
	call SetupSpellCheck()
endfunction


function FiletypeLisp()
	set expandtab
	set makeprg=sbcl\ --load\ %
endfunction


function FiletypeAssembler()
	set tabstop=8
	set shiftwidth=8
endfunction


function FiletypeForth()
	set tabstop=8
	set shiftwidth=8
	set textwidth=0
	set filetype=forth
endfunction


function FiletypeEmail()
	call FiletypeText()

	" Use LDAP lookup for completing email addresses
	function! LdapComplete(findstart, base)
		if a:findstart
			let line = getline('.')
			let start = col('.') - 1
			while start > 0 && line[start - 1] =~ '\a'
				let start -= 1
			endwhile
			return start
		else
			let output = system('/home/iclark/ldap-search.py ' . a:base)
			return split(output, '\n')
		endif
	endfunction
	setlocal completefunc=LdapComplete

	" Use tab for completion, but only on certain header lines
	function! SmartTab()
		if match(getline('.'), '^\(To\|Cc\|Bcc\):') >= 0
			return "\<C-X>\<C-U>"
		else
			return "\<tab>"
		endif
	endfunction
	inoremap <tab> <c-r>=SmartTab()<CR>

	" If the To: line is populated start on the first blank line
	if match(getline(2), '^To:.\{2,\}$') >= 0
		/^$/
		normal O
		normal o
	" otherwise start on the To: line
	else
		normal 2G
	endif
	startinsert!
endfunction


autocmd BufEnter,BufNew ~/.mutt/tmp/*		call FiletypeEmail()

autocmd BufEnter,BufNew *.py		call FiletypePython()
autocmd BufEnter,BufNew *.hs		call FiletypeHaskell()
autocmd BufEnter,BufNew *.red		call FiletypeRedcode()
autocmd BufEnter,BufNew *.*sql,*.pls	call FiletypeSQL()
autocmd BufEnter,BufNew *.txt		call FiletypeText()
autocmd BufEnter,BufNew *.c,*.h		call FiletypeC()
autocmd BufEnter,BufNew *.*tex		call FiletypeTex()
autocmd BufEnter,BufNew *.lisp,*.cl	call FiletypeLisp()
autocmd BufEnter,BufNew *.asm		call FiletypeAssembler()
autocmd BufEnter,BufNew *.f			call FiletypeForth()
autocmd	BufEnter *					call ShowErrors()
