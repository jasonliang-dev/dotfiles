#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

source $HOME/.shell-start

export PS1="\[\e[36m\]\w\[\e[m\]\\n$ "
