#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias l='ls -l'
alias la='ls -a'
alias ll='ls -al'
alias ..='cd ..'
alias v='vim'
alias c='clear'
alias x='exit'

alias pkgcount='(dpkg --list || pacman -Q) 2> /dev/null | wc -l'

export PS1="┌─[ \[\e[33m\]\w\[\e[m\] ] \n└─╼ "
