" SET LEADER

let mapleader = " "

" VUNDLE

set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'

call vundle#end()        
filetype plugin indent on

" APPEARANCE

let base16colorspace=256
colorscheme base16-default-dark
set number         " show line number
set relativenumber " enable relative numbers
set cursorline     " highlight current line
set shiftwidth=4   " affects indent width

" cursor shape based on mode
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" OTHER

" find my files better
set path+=**

" show tab results
set wildmenu

" search as I'm typing
set incsearch

" highlight as I search
set hlsearch

" clear highlighted text
nnoremap <leader><leader> :nohl<CR>

" enable folding
set foldenable

" move by visual line
nnoremap j gj
nnoremap k gk

" enable modeline
set modeline
