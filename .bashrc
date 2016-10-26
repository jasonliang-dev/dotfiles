# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source $HOME/.alias

PS1='
\e[34m\u \e[39mat \e[36m\h \e[39min \e[33m\w 
\e[34m>>\e[39m '

fortune -s
