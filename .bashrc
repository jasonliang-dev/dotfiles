# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias  ls='ls --color=auto'
alias   l='ls -l'
alias  ll='ls -la'
alias  la='ls -a'
alias  rr='rm -r'
alias  ..='cd ..'
alias ...='cd ../..'
alias   v='vim'
alias   t='tmux'
alias   c='clear'
alias   g='git'
alias   x='exit'
alias fetch='neofetch --ascii'

PS1='
\u at \h
\w \\$ '

fortune -s
