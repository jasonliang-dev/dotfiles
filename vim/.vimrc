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
" :'<,'>sort /\//
Plugin 'w0rp/ale'
Plugin 'chriskempson/base16-vim'
Plugin 'Raimondi/delimitMate'
Plugin 'elmcast/elm-vim'
Plugin 'mattn/emmet-vim'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'SirVer/ultisnips'
Plugin 'mbbill/undotree'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-surround'
Plugin 'christoomey/vim-tmux-navigator'

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
" }}}
" Appearance {{{
" use base16 colourscheme
colorscheme base16-default-dark

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
"let g:airline_powerline_fonts = 1

" separators don't play very well
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''

" change airline sections
let g:airline_section_x = '%3p%%  %2l,%2c'
let g:airline_section_y = ''
let g:airline_section_z = ''

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

" use prettier
let g:ale_fixers = {}
let g:ale_fixers['javascript'] = ['prettier']
let g:ale_javascript_prettier_options = '--single-quote --trailing-comma es5'
let g:ale_javascript_prettier_use_local_config = 1

" fix errors on save
let g:ale_fix_on_save = 1
" }}}
" emmet {{{
let g:user_emmet_leader_key='\'
let g:user_emmet_expandabbr_key = '<C-e>'
" }}}
" fugitive {{{
" bindings
nnoremap <leader>gs :Gstatus<CR>
" }}}
" fzf {{{
" show hidden files and ignore directories like `node_modules`
" let $FZF_DEFAULT_COMMAND = 'grep -ril . * .* --exclude-dir=node_modules --exclude-dir=\.git'
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git --ignore node_modules -g ""'

" map it to `;`
nnoremap ; :Files<CR>

" list buffers
nnoremap <leader>bl :Buffers<CR>

" list tags in project
nnoremap <leader>tt :Tags<CR>
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
nnoremap <leader>u :UndotreeToggle<CR>:UndotreeFocus<CR>
" }}}
" utlisnips {{{
" bindings
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" use my snippets
let g:UltiSnipsSnippetDirectories=['lia-snippets']
" }}}
" vim-javascript {{{
" temp fix for jsx brace highlight
let g:jsx_ext_required = 0
" }}}
" }}}
" Bindings {{{
" move by visual line
nnoremap j gj
nnoremap k gk

" change directory to current file
nnoremap <leader>d :lcd %:p:h<CR>

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

" clear highlighted text
nnoremap <ESC><ESC> :nohl<CR>

" switch buffers
nnoremap <leader><tab> :b#<CR>
nnoremap <leader>h :bprevious!<CR>
nnoremap <leader>l :bnext!<CR>

" delete buffer
nnoremap <leader>bd :bd<CR>

" delete all buffers
nnoremap <leader>bD :bufdo bd<CR>

" open netrw
nnoremap <leader>ff :Explore<CR>

" open terminal
nnoremap <leader><return> :!$HOME/scripts/term.sh > /dev/null 2>&1 &<CR><CR>

" open file explorer
nnoremap <leader><leader><return> :!$HOME/scripts/files.sh > /dev/null 2>&1 &<CR><CR>

" edit vimrc
nnoremap <leader>fed :e $MYVIMRC<CR>
" }}}


" vim:foldmethod=marker:foldlevel=0
