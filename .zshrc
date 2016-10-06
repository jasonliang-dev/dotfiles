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


PROMPT="
%{$fg[blue]%}%n%{$reset_color%} at %{$fg[magenta]%}%m%{$reset_color%}
%{$fg[blue]%}↪%{$reset_color%} "
RPROMPT="%{$fg[yellow]%}%~%{$reset_color%}"

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

alias    ls='ls --color=auto'
alias     l='ls -l'
alias    ll='ls -la'
alias    la='ls -a'
alias    rr='rm -r'
alias    ..='cd ..'
alias   ...='cd ../..'
alias     v='vim'
alias     t='tmux'
alias     c='clear'
alias     g='git'
alias     x='exit'
alias fetch='neofetch --ascii'

fortune -s
