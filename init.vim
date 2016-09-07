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

imap <C-l> <ESC>:w<CR>

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

filetype on

au FileType python nnoremap ,t :!python -m unittest %<CR>

call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'jiangmiao/auto-pairs'
Plug 'mxw/vim-jsx'
Plug 'kovisoft/slimv'
call plug#end()
