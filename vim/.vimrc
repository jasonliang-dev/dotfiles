" Vundle and plugins {{{
set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'kien/ctrlp.vim'
Plugin 'raimondi/delimitmate'
Plugin 'junegunn/goyo.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'valloric/youcompleteme'

" All of your Plugins must be added before the following line
call vundle#end()        
filetype plugin indent on

" Toggle NERDTree
map <C-n> :NERDTreeToggle<CR>

" Let ctrlp ignore some files
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
	    \ 'dir':  '\v[\/]\.(git|hg|svn)$',
	    \ 'file': '\v\.(exe|so|dll)$',
	    \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
	    \ }

" }}}

" Looks {{{
let base16colorspace=256
colorscheme base16-default-dark
set number         " show line number
set relativenumber " enable relative numbers
set cursorline     " highlight current line
set shiftwidth=4   " affects indent width

" Change cursor
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
" }}}

" Searching {{{
set incsearch " search as characters are entered
set hlsearch  " highlight search matches

" Clear highlighted text
nnoremap <leader><leader> :nohl<CR>
" }}}

" Folding {{{
set foldenable        " enable folding
set foldlevelstart=10 " opens most folds
set foldnestmax=10    " 10 nested folds max
set foldmethod=indent " fold based on indent

" Toggle folds
nnoremap <tab> za
" }}}

" Resize splits {{{
nnoremap <Leader><right> :vertical resize +4<CR>
nnoremap <Leader><left> :vertical resize -4<CR>
nnoremap <Leader><down> :res +4<CR>
nnoremap <Leader><up> :res -4<CR>
" }}}

" Others {{{
" Use mouse
set mouse=a
" Move vertically by visual line
nnoremap j gj
nnoremap k gk

" Highlight last inserted text
nnoremap gV `[v`]

" enable modeline
set modeline
" }}}

" vim:foldmethod=marker:foldlevel=0
