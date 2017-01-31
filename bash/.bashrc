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

alias e='emacsclient'
alias v='vim'

alias   g='git'
alias  ga='git add'
alias  gc='git commit -m'
alias  gd='git diff'
alias  gl='git log'
alias  gr='git reset HEAD'
alias  gs='git status'
alias gca='git commit --amend'

alias c='clear'
alias x='exit'

alias pkgcount='(dpkg --list || pacman -Q) 2> /dev/null | wc -l'
alias info='neofetch --color_blocks off --image /home/jason/Pictures/wallpaper.jpg'

export EDITOR='vim'
export VISUAL='vim'

export PS1="┌─[ \[\e[33m\]\w\[\e[m\] ] \n└─╼ "
