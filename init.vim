map ,e :CtrlPMixed<Enter>
map ,f :bn<CR>
map ,a :bp<CR>
map ,b :ls<CR>:b 
map ,r :vertical resize 100<CR>
map ,h <C-W>h
map ,l <C-W>l
map ,j <C-W>j
map ,k <C-W>k
map ,g <C-W>=
map ,s :grep -Rn '' ./<Left><Left><Left><Left>
map ,q :copen
map ,z y/<C-R>"<CR>
imap <C-l> <ESC>:w<CR>

" Switch to previous buffer then delete next buffer
" This closes buffer without closing split
map ,m :bp|bd #

highlight TrailSpace guibg=red ctermbg=darkred
match TrailSpace /\s\+$/

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
Plug 'jiangmiao/auto-pairs'
Plug '/pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
call plug#end()
