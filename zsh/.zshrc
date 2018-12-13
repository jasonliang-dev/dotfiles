source $HOME/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle colored-man-pages
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle mafredri/zsh-async
antigen theme denysdovhan/spaceship-prompt
antigen apply

# User configuration

export EDITOR='vi'

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

source $HOME/.alias
source $HOME/.path

(fortune -s && echo "") 2> /dev/null

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/jason/.sdkman"
# [[ -s "/home/jason/.sdkman/bin/sdkman-init.sh" ]] && source "/home/jason/.sdkman/bin/sdkman-init.sh"
