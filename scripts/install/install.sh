#!/bin/bash

sudo apt install $(< $HOME/dotfiles/packages)

$HOME/dotfiles/scripts/install/symlink.sh

git clone https://github.com/olivierverdier/zsh-git-prompt.git ~/zsh-git-prompt
git clone git://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions

$HOME/dotfiles/scripts/install/pathogen.sh
