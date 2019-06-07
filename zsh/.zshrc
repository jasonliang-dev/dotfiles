source $HOME/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle z
antigen bundle colored-man-pages
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle mafredri/zsh-async
antigen theme denysdovhan/spaceship-prompt
antigen apply

# User configuration

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $HOME/.shell-start
