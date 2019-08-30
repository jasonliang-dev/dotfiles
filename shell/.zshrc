source $HOME/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle mafredri/zsh-async
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle sindresorhus/pure
antigen apply

# User configuration

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $HOME/.shell-start
