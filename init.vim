map ,e :CtrlPMixed<Enter>
map ,f :bn<CR>
map ,a :bp<CR>
map ,b :ls<CR>:b 
map ,r :vertical resize 80<CR>
map ,x <C-W>h
map ,c <C-W>l
map ,g <C-W>=

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

set wildignore+=*/vendor/*,*/tmp/*

filetype indent on
filetype on
filetype plugin on

call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'kchmck/vim-coffee-script'
call plug#end()
