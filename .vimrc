set nocompatible " be iMproved
filetype plugin indent on

" -- BEHAVIOUR ------------------------------------------------------

" tab key inserts spaces
set expandtab

" indent size
set shiftwidth=2
set softtabstop=2

" don't auto insert comments
autocmd BufNewFile,BufRead * setlocal formatoptions-=cro

" command menu autocomplete
set wildmenu

" search as I'm typing
set incsearch

" highlight as I search
set hlsearch

" split below and to the right
set splitbelow
set splitright

" never auto indent when pasting
set nopaste

" -- APPEARANCE -----------------------------------------------------

" use custom colorscheme
colorscheme my-scheme

" error color
hi Error cterm=underline ctermfg=Red ctermbg=NONE

" bracket highlight color
hi MatchParen cterm=bold ctermbg=NONE

" search color
hi Search cterm=NONE ctermfg=Black ctermbg=Blue

" show matching parenthesis
set showmatch

" cursor shape based on mode
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" -- KEY BINDINGS ---------------------------------------------------

" leader key
let mapleader = " "

" move by visual line
nnoremap j gj
nnoremap k gk

" leader + w is C-w
nnoremap <leader>w <C-W>

" clear highlighted text
nnoremap <leader><ESC> :nohl<CR>

" switch to last buffer
nnoremap <leader><tab> :b#<CR>

