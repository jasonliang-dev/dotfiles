#!/bin/bash

source $HOME/dotfiles/scripts/ask.sh

if ask "Create symlinks?" Y; then
    $HOME/dotfiles/scripts/install/symlink.sh
fi

if ask "Install Pathogen Plugins?" Y; then
    $HOME/dotfiles/scripts/install/pathogen-install.sh
fi

if ask "Install base16-shell?" Y; then
    git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
fi

if ask "Install Termite? (Ubuntu Only)" Y; then
    $HOME/dotfiles/scripts/install/termite.sh
fi
