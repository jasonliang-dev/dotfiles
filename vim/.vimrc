" Vundle {{{
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" use the following plugins
Plugin 'SirVer/ultisnips'
Plugin 'Valloric/YouCompleteMe'
Plugin 'airblade/vim-gitgutter'
Plugin 'chriskempson/base16-vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'honza/vim-snippets'
Plugin 'mattn/emmet-vim'
Plugin 'mbbill/undotree'
Plugin 'pangloss/vim-javascript'
Plugin 'ryanoasis/vim-devicons'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'w0rp/ale'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" }}}
" Behaviour {{{
" use space as leader key
let mapleader = " "

" use modeline
set modelines=1

" tab inserts spaces
set expandtab

" indent size
set shiftwidth=2
set softtabstop=2

" file specific indentation
autocmd FileType java setlocal shiftwidth=3 softtabstop=3

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
" }}}
" Appearance {{{
" use base16 colourscheme
colorscheme base16-default-dark

" Access colors present in 256 colorspace
let base16colorspace=256

" enable syntax hiing
syntax enable

" line numbers colors
hi CursorLineNr ctermbg=NONE
hi LineNr ctermbg=NONE

" search color
hi Search cterm=NONE ctermfg=black ctermbg=blue

" use relative line numbers
set number
set relativenumber

" highlight current line
set cursorline

" show matching parenthesis
set showmatch

" always show sign column
set signcolumn=yes

" cursor shape based on mode
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
" }}}
" Plugin Configuration {{{
" airline {{{
" make airline all fancy
" requires patched font
let g:airline_powerline_fonts = 1

" change airline sections
let g:airline_section_x = '%2l,%2c'
let g:airline_section_y = ''
let g:airline_section_z = '%3p%%'

" use base16 airline theme
let g:airline_theme='base16'

" display buffers on tabline
let g:airline#extensions#tabline#enabled = 1

" show errors on airline
let g:airline#extensions#ale#enabled = 1
" }}}
" ale {{{
" change signs
let g:ale_sign_error = 'E:'
let g:ale_sign_warning = 'W:'
" }}}
" ctrlp {{{
" map CtrlP to, well uh, CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" show buffers
nnoremap <leader>bl :CtrlPBuffer<CR>

" show all
nnoremap <leader>bb :CtrlPMixed<CR>

" use ctrl-n and ctrl-p to select
let g:ctrlp_prompt_mappings = {
    \ 'PrtSelectMove("j")':   ['<c-j>', '<c-n>'],
    \ 'PrtSelectMove("k")':   ['<c-k>', '<c-p>'],
    \ 'PrtHistory(-1)':       ['<down>'],
    \ 'PrtHistory(1)':        ['<up>'],
    \ }

" show hidden files
let g:ctrlp_show_hidden = 1

" let CtrlP ignore some files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
" }}}
" fugitive {{{
" bindings
nnoremap <leader>gs :Gstatus<CR>
" }}}
" gitgutter {{{
" disable bindings
let g:gitgutter_map_keys = 0

" symbols
let g:gitgutter_sign_added = '▎'
let g:gitgutter_sign_modified = '▎'
let g:gitgutter_sign_removed = '▎'
let g:gitgutter_sign_removed_first_line = '▎'
let g:gitgutter_sign_modified_removed = '▎'

" change color
let g:gitgutter_override_sign_column_highlight = 0
hi SignColumn ctermbg=NONE
hi GitGutterAdd ctermbg=NONE
hi GitGutterChange ctermbg=NONE
hi GitGutterDelete ctermbg=NONE
hi GitGutterChangeDelete ctermbg=NONE
" }}}
" undotree {{{
nnoremap <leader>u :UndotreeToggle<CR>
" }}}
" utlisnips {{{
" bindings
let g:UltiSnipsExpandTrigger="<C-K>"
let g:UltiSnipsJumpForwardTrigger="<C-K>"
let g:UltiSnipsJumpBackwardTrigger="<S-C-K>"

" use my snippets
let g:UltiSnipsSnippetDirectories=['lia-snippets']
" }}}
" }}}
" Bindings {{{
" move by visual line
nnoremap j gj
nnoremap k gk

" copy and paste using the system clipboard
" TODO: find out why this doesn't work
nnoremap <leader>y "+y
nnoremap <leader>p "+p

" change directory to current file
nnoremap <leader>D :lcd %:p:h<CR>

" easy split navigation
nnoremap <leader>wj <C-W><C-J>
nnoremap <leader>wk <C-W><C-K>
nnoremap <leader>wl <C-W><C-L>
nnoremap <leader>wh <C-W><C-H>

" move splits
nnoremap <leader>wJ <C-W><S-J>
nnoremap <leader>wK <C-W><S-K>
nnoremap <leader>wL <C-W><S-L>
nnoremap <leader>wH <C-W><S-H>

" create splits
nnoremap <leader>ws :sp<CR>
nnoremap <leader>wv :vsp<CR>

" maximize the current window
nnoremap <leader>wo <C-W>o

" toggle spelling
nnoremap <leader>ts :set spell!<CR>

" toggle cursor highlight


" clear hied text
nnoremap <leader><leader> :nohl<CR>

" switch buffers
nnoremap <leader><Esc> :b#<CR>
nnoremap <leader><Tab> :bnext<CR>
nnoremap <leader><S-Tab> :bprevious<CR>

" delete buffer
nnoremap <leader>bd :bd<CR>

" open netrw
nnoremap <leader>ff :Explore<CR>

" edit vimrc
nnoremap <leader>fed :e $MYVIMRC<CR>
" }}}


" vim:foldmethod=marker:foldlevel=0
