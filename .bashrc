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
\e[34m\u \e[39mat \e[35m\h \e[39min \e[33m\w 
\e[34m↪\e[39m '

fortune -s
