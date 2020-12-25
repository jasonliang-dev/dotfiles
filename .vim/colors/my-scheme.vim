" color legend (from st config.h):

" 00 - black
" 01 - red
" 02 - green
" 03 - yellow
" 04 - blue
" 05 - magenta
" 06 - cyan
" 07 - gray

" 08 - light black
" 09 - light red
" 10 - light green
" 11 - light yellow
" 12 - light blue
" 13 - light magenta
" 14 - light cyan
" 15 - light gray

hi clear
syntax reset
let g:colors_name = "my-scheme"
set background=dark

hi Normal ctermfg=15 ctermbg=NONE

hi Comment ctermfg=08
hi Conditional ctermfg=03 ctermbg=00
hi Constant ctermfg=02 ctermbg=00
hi CursorLineNR ctermfg=03 ctermbg=00
hi Debug ctermfg=03 ctermbg=00
hi Define ctermfg=03 ctermbg=00
hi Delimiter ctermfg=03 ctermbg=00
hi DiffAdd ctermfg=02 ctermbg=00
hi DiffChange ctermfg=03 ctermbg=00
hi DiffDelete ctermfg=01 ctermbg=00
hi DiffText ctermfg=01 ctermbg=00
hi Directory ctermfg=05 ctermbg=00
hi Error ctermfg=01 ctermbg=00
hi ErrorMsg ctermfg=01 ctermbg=00
hi Exception ctermfg=01 ctermbg=00
hi Function ctermfg=04 ctermbg=00
hi GitGutterAdd ctermfg=02 ctermbg=00
hi GitGutterChange ctermfg=03 ctermbg=00
hi Identifier ctermfg=01 ctermbg=00
hi IncSearch ctermfg=03 ctermbg=00
hi Include ctermfg=05 ctermbg=00
hi Keyword ctermfg=02 ctermbg=00
hi Label ctermfg=03 ctermbg=00
hi Macro ctermfg=15 ctermbg=00
hi MatchParen ctermfg=03 ctermbg=00
hi MoreMsg ctermfg=03 ctermbg=00
hi NonText ctermfg=08 ctermbg=00
hi Number ctermfg=03 ctermbg=00
hi Operator ctermfg=06 ctermbg=00
hi PMenuSel ctermfg=02 ctermbg=00
hi PreCondit ctermfg=03 ctermbg=00
hi PreProc ctermfg=01 ctermbg=00
hi Repeat ctermfg=02 ctermbg=00
hi Search ctermbg=08 ctermfg=15
hi Special ctermfg=03 ctermbg=00
hi SpecialChar ctermfg=03 ctermbg=00
hi SpecialComment ctermfg=08 ctermbg=00
hi Statement ctermfg=06 ctermbg=00
hi Storage ctermfg=05 ctermbg=00
hi String ctermfg=02 ctermbg=00
hi Tag ctermfg=03 ctermbg=00
hi Title ctermfg=03 ctermbg=00
hi Todo ctermfg=03 ctermbg=00
hi Type ctermfg=04 ctermbg=00
hi WarningMsg ctermfg=01 ctermbg=00
hi cssAttr ctermfg=06 ctermbg=00
hi cssClassName ctermfg=05 ctermbg=00
hi cssClassNameDot ctermfg=05 ctermbg=00
hi cssColor ctermfg=03 ctermbg=00
hi cssIdentifier ctermfg=01 ctermbg=00
hi cssImportant ctermfg=01 ctermbg=00
hi cssIncludeKeyword ctermfg=02 ctermbg=00
hi javaScriptBoolean ctermfg=05 ctermbg=00
hi markdownLinkText ctermfg=05 ctermbg=00

