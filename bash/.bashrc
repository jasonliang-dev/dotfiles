#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source $HOME/.alias

export EDITOR='vim'
export VISUAL='vim'

export PS1="\[\e[36m\]\w\[\e[m\]\\n> "
BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

(fortune -s && echo "") 2> /dev/null
