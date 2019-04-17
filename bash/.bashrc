#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source $HOME/.shell-start

export EDITOR='vim'
export VISUAL='vim'

export PS1="\[\e[36m\]\w\[\e[m\]\\n$ "
