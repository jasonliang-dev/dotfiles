" Pathogen and plugins {{{
" Run pathogen
execute pathogen#infect()

filetype plugin on
" Toggle NERDTree
map <C-n> :NERDTreeToggle<CR>

" Exclude some files
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn)$',
    \ 'file': '\v\.(exe|so|dll)$',
    \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
    \ }

" Pencel
let g:pencil#wrapModeDefault = 'soft'
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType text         call pencil#init()
augroup END
" }}}

" Colors {{{
syntax enable                   " enable highlighting
let base16colorspace=256
colorscheme base16-default-dark " colorscheme
" }}}

" UI {{{
set number         " show line number
set relativenumber " enable relative numbers
set showcmd        " show command in the bottom bar
set cursorline     " highlight current line
filetype indent on " loads indent files
set expandtab      " tabs become spaces
set shiftwidth=4   " affects automatic indentation
set wildmenu       " autocomplete for command menu
set showmatch      " highlight matching brackets
" }}}

" Change cursor shape {{{
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
" }}}

" Searching {{{
set incsearch " search as characters are entered
set hlsearch " highlight search matches
" Clear highlighted text
nnoremap <silent> <leader><space> :nohl<CR>
" }}}

" Folding {{{
set foldenable        " enable folding
set foldlevelstart=10 " opens most folds
set foldnestmax=10    " 10 nested folds max
set foldmethod=indent " fold based on indent
" Toggle folds
nnoremap <space> za
" }}}

" Others {{{
" Move vertically by visual line
nnoremap j gj
nnoremap k gk
" Highlight last inserted text
nnoremap gV `[v`]
" }}}

" Resize splits quickly {{{
nnoremap <silent> <Leader>. :vertical resize +4<CR>
nnoremap <silent> <Leader>, :vertical resize -4<CR>
nnoremap <silent> <Leader>= :res +4<CR>
nnoremap <silent> <Leader>- :res -4<CR>
" }}}

" Mode line
set modeline

" vim: foldmethod=marker:foldlevel=0
