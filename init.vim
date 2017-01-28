syntax on

map ,e :CtrlP<Enter>
map ,m :CtrlPMixed<CR>
map ,f :bn<CR>
map ,a :bp<CR>
map ,b :ls<CR>:b
map ,r :vertical resize 100<CR>
map ,h <C-W>h
map ,l <C-W>l
map ,j <C-W>j
map ,k <C-W>k
map ,g <C-W>=
map ,s :grep -Rn --exclude="tags" '' ./<Left><Left><Left><Left>
map ,q :copen
map ,z y/<C-R>"<CR>
map ,v :w<cr>
map <C-O> :Explore<CR>

" Delete trailing whitespaces
map ,w :%s/\s\+$//e<CR>

imap <C-l> <ESC>:w<CR>

" Remove whitespace
nnoremap ,w :%s/\s\+$//e<CR>

" Navigation
map <c-j> 10j
map <c-k> 10k

" Switch to previous buffer then delete next buffer
" This closes buffer without closing split
map ,d :bd#<CR>

highlight TrailSpace guibg=red ctermbg=darkred
match TrailSpace /\s\+$/

set dir=~/.tmp/swap
set backup
set backupdir=~/.tmp/backup
set undofile
set undodir=~/.tmp/undo/

set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set showmatch
set nohlsearch
set ruler
set number

set wildignore+=*/vendor/*,*/tmp/*
set wildignore+=*\.pyc
set wildignore+=tags

filetype on

au FileType python nnoremap ,t :!python -m unittest %<CR>

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'jiangmiao/auto-pairs'
Plug 'mxw/vim-jsx'
Plug 'kovisoft/slimv'
Plug 'tpope/vim-obsession'
Plug 'vim-syntastic/syntastic'
call plug#end()


" *** FROM python.org vimrc ***
" vimrc file for following the coding standards specified in PEP 7 & 8.
"
" To use this file, source it in your own personal .vimrc file (``source
" <filename>``) or, if you don't have a .vimrc file, you can just symlink to it
" (``ln -s <this file> ~/.vimrc``).  All options are protected by autocmds
" (read below for an explanation of the command) so blind sourcing of this file
" is safe and will not affect your settings for non-Python or non-C files.
"
"
" All setting are protected by 'au' ('autocmd') statements.  Only files ending
" in .py or .pyw will trigger the Python settings while files ending in *.c or
" *.h will trigger the C settings.  This makes the file "safe" in terms of only
" adjusting settings for Python and C files.
"
" Only basic settings needed to enforce the style guidelines are set.
" Some suggested options are listed but commented out at the end of this file.

" Number of spaces that a pre-existing tab is equal to.
" For the amount of space used for a new tab use shiftwidth.
au BufRead,BufNewFile *py,*pyw,*.c,*.h set tabstop=8

" What to use for an indent.
" This will affect Ctrl-T and 'autoindent'.
" Python: 4 spaces
" C: tabs (pre-existing files) or 4 spaces (new files)
au BufRead,BufNewFile *.py,*pyw set shiftwidth=4
au BufRead,BufNewFile *.py,*.pyw set expandtab
fu Select_c_style()
    if search('^\t', 'n', 150)
        set shiftwidth=8 noexpandtab
    el
        set shiftwidth=4 expandtab
    en
endf
au BufRead,BufNewFile *.c,*.h call Select_c_style()
au BufRead,BufNewFile Makefile* set noexpandtab

" Use the below highlight group when displaying bad whitespace is desired.
highlight BadWhitespace ctermbg=red guibg=red

" Display tabs at the beginning of a line in Python mode as bad.
au BufRead,BufNewFile *.py,*.pyw match BadWhitespace /^\t\+/
" Make trailing whitespace be flagged as bad.
au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

" Wrap text after a certain number of characters
" Python: 79
" C: 79
" au BufRead,BufNewFile *.py,*.pyw,*.c,*.h set textwidth=79

" Turn off settings in 'formatoptions' relating to comment formatting.
" - c : do not automatically insert the comment leader when wrapping based on
"    'textwidth'
" - o : do not insert the comment leader when using 'o' or 'O' from command mode
" - r : do not insert the comment leader when hitting <Enter> in insert mode
" Python: not needed
" C: prevents insertion of '*' at the beginning of every line in a comment
au BufRead,BufNewFile *.c,*.h set formatoptions-=c formatoptions-=o formatoptions-=r

" Use UNIX (\n) line endings.
" Only used for new files so as to not force existing files to change their
" line endings.
" Python: yes
" C: yes
au BufNewFile *.py,*.pyw,*.c,*.h set fileformat=unix


" ----------------------------------------------------------------------------
" The following section contains suggested settings.  While in no way required
" to meet coding standards, they are helpful.

" Set the default file encoding to UTF-8: ``set encoding=utf-8``

" Puts a marker at the beginning of the file to differentiate between UTF and
" UCS encoding (WARNING: can trick shells into thinking a text file is actually
" a binary file when executing the text file): ``set bomb``

" For full syntax highlighting:
"``let python_highlight_all=1``
"``syntax on``

" Automatically indent based on file type: ``filetype indent on``
" Keep indentation level from previous line: ``set autoindent``

" Folding based on indentation: ``set foldmethod=indent``
