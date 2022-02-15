set nocompatible
filetype plugin indent on

" -- PLUGINS --------------------------------------------------------

call plug#begin()
Plug 'chriskempson/base16-vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'mattn/emmet-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tikhomirov/vim-glsl'
Plug 'tpope/vim-surround'
call plug#end()

" -- APPEARANCE -----------------------------------------------------

" hide gui bars (gvim)
set guioptions-=m
set guioptions-=r
set guioptions-=L
set guioptions-=T

" display tab characters
set list
set listchars=tab:»\ 

" do not shift text
set signcolumn=yes

" jsonc comments
autocmd FileType json syntax match Comment +\/\/.\+$+

" font
set guifont=Inconsolata:h12:b

" syntax highlighting
syntax enable

" color theme
silent! colorscheme base16-eighties
hi LineNr     guibg=NONE ctermbg=00
hi SignColumn guibg=NONE ctermbg=00
hi MatchParen guibg=NONE ctermbg=00

" -- BEHAVIOUR ------------------------------------------------------

" don't auto insert comments
autocmd BufNewFile,BufRead,VimEnter * setl fo-=cro

" indent size
set shiftwidth=4
set softtabstop=4
set tabstop=4

" tab key inserts spaces
set expandtab

" never auto indent when pasting
set nopaste

" command menu autocomplete
set wildmenu

" search while typing
set incsearch

" highlight while typing
set hlsearch

" split below and to the right
set splitbelow
set splitright

" mouse support
set mouse=a

" system clipboard
set clipboard+=unnamedplus

" shorten default update time
set updatetime=300

" -- KEY BINDINGS ---------------------------------------------------

" leader key
let mapleader = " "

" move by visual line
nnoremap j gj
nnoremap k gk

" come on, ctrl+s doesn't do anything
nnoremap <C-s> :w<CR>

" same with ctrl+c, ctrl+v
vnoremap <C-c> "+y
inoremap <C-v> <C-r>+
cnoremap <C-v> <C-r>+

" terminal
nnoremap <leader><CR> :terminal<CR>

" in terminal, esc returns to normal mode
tnoremap <Esc> <C-\><C-n>

" toggle line numbers
nnoremap <leader>l :set number!<CR>
nnoremap <leader>r :set number! relativenumber!<CR>

" toggle tabs/spaces
nnoremap <leader>t :set expandtab? expandtab!<CR>

" set indent size
nnoremap <leader>2 :set shiftwidth=2 softtabstop=2 tabstop=2<CR>
nnoremap <leader>3 :set shiftwidth=3 softtabstop=3 tabstop=3<CR>
nnoremap <leader>4 :set shiftwidth=4 softtabstop=4 tabstop=4<CR>
nnoremap <leader>5 :set shiftwidth=5 softtabstop=5 tabstop=5<CR>
nnoremap <leader>6 :set shiftwidth=6 softtabstop=6 tabstop=6<CR>
nnoremap <leader>7 :set shiftwidth=7 softtabstop=7 tabstop=7<CR>
nnoremap <leader>8 :set shiftwidth=8 softtabstop=8 tabstop=8<CR>

" clear highlighted text
nnoremap <leader><ESC> :nohl<CR>

" window
nnoremap <leader>w <C-w>

" delete buffer
nnoremap <leader>k :bp\|bd #<CR>

" switch to last buffer
nnoremap <leader><tab> :b#<CR>

" close window
nnoremap <C-q> :q<CR>

" change directory
nnoremap <leader>d :cd %:h<CR>

" search buffers
nnoremap <leader>b :CtrlPBuffer<CR>

" search most recently used
nnoremap <leader>f :CtrlPMRUFiles<CR>

" coc
nmap <BS>e     :CocDiagnostics<CR>
nmap <BS><BS>  <Plug>(coc-definition)
nmap <BS>f     <Plug>(coc-fix-current)
nmap <BS>r     <Plug>(coc-rename)
nmap <leader>= <Plug>(coc-format)

" change ctrlp up/down
let g:ctrlp_prompt_mappings = {
  \ 'PrtSelectMove("j")': ['<c-n>', '<down>'],
  \ 'PrtSelectMove("k")': ['<c-p>', '<up>'],
  \ 'PrtHistory(-1)':     ['<c-j>'],
  \ 'PrtHistory(1)':      ['<c-k>'],
  \ }
