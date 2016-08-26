" Pathogen {{{
execute pathogen#infect()
" }}}

" Colors {{{
syntax enable                   " enable highlighting
set background=dark 
colorscheme base16-default-dark " colorscheme
" }}}

" UI {{{
set number         " show line number
set relativenumber " enable relative numbers
set showcmd        " show command in the bottom bar
set cursorline     " highlight current line
filetype indent on " loads indent files
set expandtab      " tabs become spaces
set shiftwidth=2   " affects automatic indentation
set wildmenu       " autocomplete for command menu
set showmatch      " highlight matching brackets
set guifont=hack\ 12
" }}}

" Searching {{{
set incsearch " search as characters are entered
set hlsearch " highlight search matches
" clear hilighted text
nnoremap <silent> <leader><space> :nohl<CR>
" }}}

" Folding {{{
set foldenable        " enable folding
set foldlevelstart=10 " opens most folds
set foldnestmax=10    " 10 nested folds max
set foldmethod=indent " fold based on indent
" toggle folds
nnoremap <space> za
" }}}

" Movement {{{
" move vertically by visual line
nnoremap j gj
nnoremap k gk
" highlight last inserted text
nnoremap gV `[v`]
" }}}

" Splits {{{
" resize splits quickly
nnoremap <silent> <Leader>. :vertical resize +4<CR>
nnoremap <silent> <Leader>, :vertical resize -4<CR>
nnoremap <silent> <Leader>= :res +4<CR>
nnoremap <silent> <Leader>- :res -4<CR>
" }}}

" Mode line {{{
set modeline
" vim:foldmethod=marker:foldlevel=0
" }}}
