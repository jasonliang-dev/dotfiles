source $HOME/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions
antigen theme https://github.com/denysdovhan/spaceship-prompt spaceship
antigen apply

# User configuration

export EDITOR='vi'

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

source $HOME/.alias

(fortune -s && echo "") 2> /dev/null
