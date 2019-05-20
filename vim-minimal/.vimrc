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

" remove netrw banner
let g:netrw_banner = 0

" -- APPEARANCE -----------------------------------------------------

" Access colors present in 256 colorspace
let base16colorspace=256

" enable syntax hiing
syntax enable

" error color
hi Error cterm=underline ctermfg=Red ctermbg=NONE

" line numbers colors
hi CursorLineNr ctermbg=NONE
hi LineNr ctermbg=NONE

" bracket highlight color
hi MatchParen cterm=bold ctermbg=NONE

" search color
hi Search cterm=NONE ctermfg=Black ctermbg=Blue

" use relative line numbers
set number
set relativenumber

" show matching parenthesis
set showmatch

" cursor shape based on mode
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
" error color
hi Error cterm=underline ctermfg=Red ctermbg=NONE

" line numbers colors
hi CursorLineNr ctermbg=NONE
hi LineNr ctermbg=NONE

" bracket highlight color
hi MatchParen cterm=bold ctermbg=NONE

" search color
hi Search cterm=NONE ctermfg=Black ctermbg=Blue

" use relative line numbers
set number
set relativenumber
" error color
hi Error cterm=underline ctermfg=Red ctermbg=NONE

" line numbers colors
hi CursorLineNr ctermbg=NONE
hi LineNr ctermbg=NONE

" bracket highlight color
hi MatchParen cterm=bold ctermbg=NONE

" search color
hi Search cterm=NONE ctermfg=Black ctermbg=Blue

" use relative line numbers
set number
set relativenumber

" -- KEY BINDINGS ---------------------------------------------------

" leader key
let mapleader = " "

" move by visual line
nnoremap j gj
nnoremap k gk

" change working directory to current file
nnoremap <leader>dd :lcd %:p:h<CR>

" easy split navigation
nnoremap <leader>wj <C-W><C-J>
nnoremap <leader>wk <C-W><C-K>
nnoremap <leader>wl <C-W><C-L>
nnoremap <leader>wh <C-W><C-H>

" create splits
nnoremap <leader>ws :sp<CR>
nnoremap <leader>wv :vsp<CR>

" leader + w is C-w
nnoremap <leader>w <C-W>

" toggle spelling
nnoremap <leader>ts :set spell!<CR>

" clear highlighted text
nnoremap <leader><ESC> :nohl<CR>

" close buffer
nnoremap <leader>q :q<CR>

" list buffers
nnoremap <Leader>b :ls<CR>:b<Space>

" switch to last buffer
nnoremap <leader><tab> :b#<CR>

" delete buffer
nnoremap <leader>k :bd<CR>

" open netrw
nnoremap <leader>f :Explore<CR>

" edit vimrc
nnoremap <leader>1 :e $MYVIMRC<CR>
