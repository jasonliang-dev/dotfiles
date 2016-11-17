#!/bin/bash

source $HOME/dotfiles/scripts/ask.sh

if ask "Install more symlinks?" N; then
    $HOME/dotfiles/scripts/install/extra-symlink.sh
fi

if ask "Install Better Defaults for Emacs?" N; then
    git clone https://github.com/technomancy/better-defaults.git ~/.emacs.d/better-defaults
fi

if ask "Install base16-shell?" N; then
    git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
fi

if ask "Install xft port of lemonbar?" N; then
    git clone https://github.com/krypt-n/bar.git && cd bar
    sudo apt install libxcb-xinerama0-dev
    make
    sudo make install
fi

if ask "Install Termite?" N; then
    $HOME/dotfiles/scripts/install/termite.sh
fi
