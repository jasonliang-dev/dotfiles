#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source $HOME/.alias

export EDITOR='vim'
export VISUAL='vim'

export PS1="\[\e[36m\]\w\[\e[m\]\\n$ "
BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# calculator
function calc {
    echo "${1}" | bc -l;
}

(fortune -s && echo "") 2> /dev/null

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/jason/.sdkman"
[[ -s "/home/jason/.sdkman/bin/sdkman-init.sh" ]] && source "/home/jason/.sdkman/bin/sdkman-init.sh"
