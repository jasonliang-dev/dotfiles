# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jason/.zshrc'
autoload -Uz compinit
compinit
# End of lines added by compinstall
autoload -Uz colors && colors
autoload -Uz promptinit
promptinit

source ~/zsh-git-prompt/zshrc.sh
#source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

PROMPT='
%{$fg[blue]%}%n%{$reset_color%} at %{$fg[cyan]%}%m%{$reset_color%} $(git_super_status)
%{$fg[blue]%}↪%{$reset_color%} '
RPROMPT='%{$fg[yellow]%}%~%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="on "
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_SEPARATOR=""
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[magenta]%}%{%G%} "
ZSH_THEME_GIT_PROMPT_STAGED=" %{$fg[red]%}%{●%G%} "
ZSH_THEME_GIT_PROMPT_CONFLICTS=" %{$fg[red]%}%{✖%G%} "
ZSH_THEME_GIT_PROMPT_CHANGED=" %{$fg[blue]%}%{✚%G%} "
ZSH_THEME_GIT_PROMPT_BEHIND=" %{↓%G%}"
ZSH_THEME_GIT_PROMPT_AHEAD=" %{↑%G%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED=" %{…%G%}"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg_bold[green]%}%{✔%G%} "

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

source $HOME/.alias
fortune -s
