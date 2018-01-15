let mapleader = " "

set number         " show line number
set relativenumber " enable relative numbers
set shiftwidth=4   " affects indent width

" cursor shape based on mode
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" search as I'm typing
set incsearch

" highlight as I search
set hlsearch

" clear highlighted text
nnoremap <leader><leader> :nohl<CR>

" move by visual line
nnoremap j gj
nnoremap k gk
