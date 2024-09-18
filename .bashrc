# exit non-interactive shell
[[ $- != *i* ]] && return

export EDITOR='vim'
export VISUAL='vim'

alias ls="ls --color=auto"

if [[ -z ${CONTAINER_ID} ]]; then 
    export PS1="\n\[\e[34m\]\w\[\e[m\]\\n$ "
else
    export PS1="\n\[\e[31m\]\w\[\e[m\]\\n📦 "
fi
