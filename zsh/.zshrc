source $HOME/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions
antigen theme caiogondim/bullet-train-oh-my-zsh-theme bullet-train
antigen apply

# User configuration

BULLETTRAIN_PROMPT_ORDER=(
  dir
  git
)

BULLETTRAIN_DIR_FG=black

export EDITOR='vim'

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

source $HOME/.alias

(fortune -s && echo "") 2> /dev/null