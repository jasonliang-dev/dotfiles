# exit non-interactive shell
[[ $- != *i* ]] && return

export PS1="\n\[\e[36m\]\w\[\e[m\]\\n$ "
