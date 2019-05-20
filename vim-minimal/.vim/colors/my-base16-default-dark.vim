" vi:syntax=vim

" a stripped down version of base16 for terminal use only

" base16-vim (https://github.com/chriskempson/base16-vim)
" by Chris Kempson (http://chriskempson.com)
" Default Dark scheme by Chris Kempson (http://chriskempson.com)

" This enables the coresponding base16-shell script to run so that
" :colorscheme works in terminals supported by base16-shell scripts
" User must set this variable in .vimrc
"   let g:base16_shell_path=base16-builder/output/shell/
if !has('gui_running')
  if exists("g:base16_shell_path")
    execute "silent !/bin/sh ".g:base16_shell_path."/base16-default-dark.sh"
  endif
endif

" Terminal color definitions
let s:cterm00        = "00"
let g:base16_cterm00 = "00"
let s:cterm03        = "08"
let g:base16_cterm03 = "08"
let s:cterm05        = "07"
let g:base16_cterm05 = "07"
let s:cterm07        = "15"
let g:base16_cterm07 = "15"
let s:cterm08        = "01"
let g:base16_cterm08 = "01"
let s:cterm0A        = "03"
let g:base16_cterm0A = "03"
let s:cterm0B        = "02"
let g:base16_cterm0B = "02"
let s:cterm0C        = "06"
let g:base16_cterm0C = "06"
let s:cterm0D        = "04"
let g:base16_cterm0D = "04"
let s:cterm0E        = "05"
let g:base16_cterm0E = "05"
if exists('base16colorspace') && base16colorspace == "256"
  let s:cterm01        = "18"
  let g:base16_cterm01 = "18"
  let s:cterm02        = "19"
  let g:base16_cterm02 = "19"
  let s:cterm04        = "20"
  let g:base16_cterm04 = "20"
  let s:cterm06        = "21"
  let g:base16_cterm06 = "21"
  let s:cterm09        = "16"
  let g:base16_cterm09 = "16"
  let s:cterm0F        = "17"
  let g:base16_cterm0F = "17"
else
  let s:cterm01        = "10"
  let g:base16_cterm01 = "10"
  let s:cterm02        = "11"
  let g:base16_cterm02 = "11"
  let s:cterm04        = "12"
  let g:base16_cterm04 = "12"
  let s:cterm06        = "13"
  let g:base16_cterm06 = "13"
  let s:cterm09        = "09"
  let g:base16_cterm09 = "09"
  let s:cterm0F        = "14"
  let g:base16_cterm0F = "14"
endif

" let g:terminal_ansi_colors = [
"       \ "#181818",
"       \ "#ab4642",
"       \ "#a1b56c",
"       \ "#f7ca88",
"       \ "#7cafc2",
"       \ "#ba8baf",
"       \ "#86c1b9",
"       \ "#d8d8d8",
"       \ "#585858",
"       \ "#ab4642",
"       \ "#a1b56c",
"       \ "#f7ca88",
"       \ "#7cafc2",
"       \ "#ba8baf",
"       \ "#86c1b9",
"       \ "#f8f8f8",
"       \ ]

" Theme setup
hi clear
syntax reset
let g:colors_name = "my-base16-default-dark"

" Vim editor colors
exec "hi Normal"                     . " ctermfg=" . s:cterm05 . " ctermbg=" . s:cterm00
exec "hi Bold"                       . " ctermfg=" . "NONE"    . " ctermbg=" . "NONE"
exec "hi Debug"                      . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi Directory"                  . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi Error"                      . " ctermfg=" . s:cterm00 . " ctermbg=" . s:cterm08
exec "hi ErrorMsg"                   . " ctermfg=" . s:cterm08 . " ctermbg=" . s:cterm00
exec "hi Exception"                  . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi FoldColumn"                 . " ctermfg=" . s:cterm0C . " ctermbg=" . s:cterm01
exec "hi Folded"                     . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi IncSearch"                  . " ctermfg=" . s:cterm01 . " ctermbg=" . s:cterm09
exec "hi Italic"                     . " ctermfg=" . "NONE"    . " ctermbg=" . "NONE"
exec "hi Macro"                      . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi MatchParen"                 . " ctermfg=" . "NONE"    . " ctermbg=" . s:cterm03
exec "hi ModeMsg"                    . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi MoreMsg"                    . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi Question"                   . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi Search"                     . " ctermfg=" . s:cterm01 . " ctermbg=" . s:cterm0A
exec "hi Substitute"                 . " ctermfg=" . s:cterm01 . " ctermbg=" . s:cterm0A
exec "hi SpecialKey"                 . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi TooLong"                    . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi Underlined"                 . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi Visual"                     . " ctermfg=" . "NONE"    . " ctermbg=" . s:cterm02
exec "hi VisualNOS"                  . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi WarningMsg"                 . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi WildMenu"                   . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi Title"                      . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi Conceal"                    . " ctermfg=" . s:cterm0D . " ctermbg=" . s:cterm00
exec "hi Cursor"                     . " ctermfg=" . s:cterm00 . " ctermbg=" . s:cterm05
exec "hi NonText"                    . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi LineNr"                     . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi SignColumn"                 . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi StatusLine"                 . " ctermfg=" . s:cterm04 . " ctermbg=" . s:cterm02
exec "hi StatusLineNC"               . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi VertSplit"                  . " ctermfg=" . s:cterm02 . " ctermbg=" . s:cterm02
exec "hi ColorColumn"                . " ctermfg=" . "NONE"    . " ctermbg=" . s:cterm01
exec "hi CursorColumn"               . " ctermfg=" . "NONE"    . " ctermbg=" . s:cterm01
exec "hi CursorLine"                 . " ctermfg=" . "NONE"    . " ctermbg=" . s:cterm01
exec "hi CursorLineNr"               . " ctermfg=" . s:cterm04 . " ctermbg=" . s:cterm01
exec "hi QuickFixLine"               . " ctermfg=" . "NONE"    . " ctermbg=" . s:cterm01
exec "hi PMenu"                      . " ctermfg=" . s:cterm05 . " ctermbg=" . s:cterm01
exec "hi PMenuSel"                   . " ctermfg=" . s:cterm01 . " ctermbg=" . s:cterm05
exec "hi TabLine"                    . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi TabLineFill"                . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi TabLineSel"                 . " ctermfg=" . s:cterm0B . " ctermbg=" . s:cterm01

" Standard syntax highlighting
exec "hi Boolean"                    . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi Character"                  . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi Comment"                    . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi Conditional"                . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi Constant"                   . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi Define"                     . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi Delimiter"                  . " ctermfg=" . s:cterm0F . " ctermbg=" . "NONE"
exec "hi Float"                      . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi Function"                   . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi Identifier"                 . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi Include"                    . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi Keyword"                    . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi Label"                      . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi Number"                     . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi Operator"                   . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi PreProc"                    . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi Repeat"                     . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi Special"                    . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"
exec "hi SpecialChar"                . " ctermfg=" . s:cterm0F . " ctermbg=" . "NONE"
exec "hi Statement"                  . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi StorageClass"               . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi String"                     . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi Structure"                  . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi Tag"                        . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi Todo"                       . " ctermfg=" . s:cterm0A . " ctermbg=" . s:cterm01
exec "hi Type"                       . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi Typedef"                    . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"

" C highlighting
exec "hi cOperator"                  . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"
exec "hi cPreCondit"                 . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"

" C# highlighting
exec "hi csClass"                    . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi csAttribute"                . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi csModifier"                 . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi csType"                     . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi csUnspecifiedStatement"     . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi csContextualStatement"      . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi csNewDecleration"           . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"

" CSS highlighting
exec "hi cssBraces"                  . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi cssClassName"               . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi cssColor"                   . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"

" Diff highlighting
exec "hi DiffAdd"                    . " ctermfg=" . s:cterm0B . " ctermbg=" . s:cterm01
exec "hi DiffChange"                 . " ctermfg=" . s:cterm03 . " ctermbg=" . s:cterm01
exec "hi DiffDelete"                 . " ctermfg=" . s:cterm08 . " ctermbg=" . s:cterm01
exec "hi DiffText"                   . " ctermfg=" . s:cterm0D . " ctermbg=" . s:cterm01
exec "hi DiffAdded"                  . " ctermfg=" . s:cterm0B . " ctermbg=" . s:cterm00
exec "hi DiffFile"                   . " ctermfg=" . s:cterm08 . " ctermbg=" . s:cterm00
exec "hi DiffNewFile"                . " ctermfg=" . s:cterm0B . " ctermbg=" . s:cterm00
exec "hi DiffLine"                   . " ctermfg=" . s:cterm0D . " ctermbg=" . s:cterm00
exec "hi DiffRemoved"                . " ctermfg=" . s:cterm08 . " ctermbg=" . s:cterm00

" Git highlighting
exec "hi gitcommitOverflow"          . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi gitcommitSummary"           . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi gitcommitComment"           . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi gitcommitUntracked"         . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi gitcommitDiscarded"         . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi gitcommitSelected"          . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi gitcommitHeader"            . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi gitcommitSelectedType"      . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi gitcommitUnmergedType"      . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi gitcommitDiscardedType"     . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi gitcommitBranch"            . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi gitcommitUntrackedFile"     . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi gitcommitUnmergedFile"      . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi gitcommitDiscardedFile"     . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi gitcommitSelectedFile"      . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"

" GitGutter highlighting
exec "hi GitGutterAdd"               . " ctermfg=" . s:cterm0B . " ctermbg=" . s:cterm01
exec "hi GitGutterChange"            . " ctermfg=" . s:cterm0D . " ctermbg=" . s:cterm01
exec "hi GitGutterDelete"            . " ctermfg=" . s:cterm08 . " ctermbg=" . s:cterm01
exec "hi GitGutterChangeDelete"      . " ctermfg=" . s:cterm0E . " ctermbg=" . s:cterm01

" HTML highlighting
exec "hi htmlBold"                   . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi htmlItalic"                 . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi htmlEndTag"                 . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi htmlTag"                    . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"

" JavaScript highlighting
exec "hi javaScript"                 . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi javaScriptBraces"           . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi javaScriptNumber"           . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
" pangloss/vim-javascript highlighting
exec "hi jsOperator"                 . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi jsStatement"                . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi jsReturn"                   . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi jsThis"                     . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi jsClassDefinition"          . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi jsFunction"                 . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi jsFuncName"                 . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi jsFuncCall"                 . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi jsClassFuncName"            . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi jsClassMethodType"          . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi jsRegexpString"             . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"
exec "hi jsGlobalObjects"            . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi jsGlobalNodeObjects"        . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi jsExceptions"               . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi jsBuiltins"                 . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"

" Mail highlighting
exec "hi mailQuoted1"                . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi mailQuoted2"                . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi mailQuoted3"                . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi mailQuoted4"                . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"
exec "hi mailQuoted5"                . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi mailQuoted6"                . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi mailURL"                    . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi mailEmail"                  . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"

" Markdown highlighting
exec "hi markdownCode"               . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi markdownError"              . " ctermfg=" . s:cterm05 . " ctermbg=" . s:cterm00
exec "hi markdownCodeBlock"          . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi markdownHeadingDelimiter"   . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"

" NERDTree highlighting
exec "hi NERDTreeDirSlash"           . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi NERDTreeExecFile"           . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"

" PHP highlighting
exec "hi phpMemberSelector"          . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi phpComparison"              . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi phpParent"                  . " ctermfg=" . s:cterm05 . " ctermbg=" . "NONE"
exec "hi phpMethodsVar"              . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"

" Python highlighting
exec "hi pythonOperator"             . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi pythonRepeat"               . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi pythonInclude"              . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi pythonStatement"            . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"

" Ruby highlighting
exec "hi rubyAttribute"              . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"
exec "hi rubyConstant"               . " ctermfg=" . s:cterm0A . " ctermbg=" . "NONE"
exec "hi rubyInterpolationDelimiter" . " ctermfg=" . s:cterm0F . " ctermbg=" . "NONE"
exec "hi rubyRegexp"                 . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"
exec "hi rubySymbol"                 . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi rubyStringDelimiter"        . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"

" SASS highlighting
exec "hi sassidChar"                 . " ctermfg=" . s:cterm08 . " ctermbg=" . "NONE"
exec "hi sassClassChar"              . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi sassInclude"                . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi sassMixing"                 . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi sassMixinName"              . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"

" Signify highlighting
exec "hi SignifySignAdd"             . " ctermfg=" . s:cterm0B . " ctermbg=" . s:cterm01
exec "hi SignifySignChange"          . " ctermfg=" . s:cterm0D . " ctermbg=" . s:cterm01
exec "hi SignifySignDelete"          . " ctermfg=" . s:cterm08 . " ctermbg=" . s:cterm01

" Spelling highlighting
exec "hi SpellBad"                   . " ctermfg=" . "NONE"    . " ctermbg=" . "NONE"
exec "hi SpellLocal"                 . " ctermfg=" . "NONE"    . " ctermbg=" . "NONE"
exec "hi SpellCap"                   . " ctermfg=" . "NONE"    . " ctermbg=" . "NONE"
exec "hi SpellRare"                  . " ctermfg=" . "NONE"    . " ctermbg=" . "NONE"

" Startify highlighting
exec "hi StartifyBracket"            . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi StartifyFile"               . " ctermfg=" . s:cterm07 . " ctermbg=" . "NONE"
exec "hi StartifyFooter"             . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi StartifyHeader"             . " ctermfg=" . s:cterm0B . " ctermbg=" . "NONE"
exec "hi StartifyNumber"             . " ctermfg=" . s:cterm09 . " ctermbg=" . "NONE"
exec "hi StartifyPath"               . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi StartifySection"            . " ctermfg=" . s:cterm0E . " ctermbg=" . "NONE"
exec "hi StartifySelect"             . " ctermfg=" . s:cterm0C . " ctermbg=" . "NONE"
exec "hi StartifySlash"              . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"
exec "hi StartifySpecial"            . " ctermfg=" . s:cterm03 . " ctermbg=" . "NONE"

" Java highlighting
exec "hi javaOperator"               . " ctermfg=" . s:cterm0D . " ctermbg=" . "NONE"

" Remove color variables
unlet s:cterm00 s:cterm01 s:cterm02 s:cterm03 s:cterm04 s:cterm05 s:cterm06 s:cterm07 s:cterm08 s:cterm09 s:cterm0A s:cterm0B s:cterm0C s:cterm0D s:cterm0E s:cterm0F
