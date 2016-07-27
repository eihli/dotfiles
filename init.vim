map ,e :CtrlPMixed<Enter>
map ,f :bn<CR>
map ,a :bp<CR>
map ,b :ls<CR>:b 
map ,r :vertical resize 80<CR>
map ,x <C-W>h
map ,c <C-W>l
map ,g <C-W>=
map ,s :grep -Rn '' ./<Left><Left><Left><Left>
map ,q :copen
map ,z y/<C-R>"<CR>


set dir=~/.tmp/swap
set backup
set backupdir=~/.tmp/backup
set undofile
set undodir=~/.tmp/undo/

set tabstop=2
set softtabstop=4
set shiftwidth=2
set expandtab
set smarttab
set ai
set showmatch
set nohlsearch
set smartindent
set ruler
set number

set wildignore+=*/vendor/*,*/tmp/*

filetype indent on
filetype on
filetype plugin on

call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'kchmck/vim-coffee-script'
call plug#end()
