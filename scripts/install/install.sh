#!/bin/bash

source $HOME/dotfiles/scripts/ask.sh

if ask "Create Symlinks?" Y; then
    $HOME/dotfiles/scripts/install/symlink.sh
fi

if ask "Install Zsh extras?" Y; then
    git clone https://github.com/olivierverdier/zsh-git-prompt.git ~/zsh-git-prompt
    git clone git://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
fi

if ask "Install Pathogen?" Y; then
    mkdir -p ~/.vim/autoload ~/.vim/bundle
    curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

    git clone https://github.com/chriskempson/base16-vim ~/.vim/bundle/base16-vim
    git clone https://github.com/ctrlpvim/ctrlp.vim.git ~/.vim/bundle/ctrlp.vim
    git clone https://github.com/raimondi/delimitmate ~/.vim/bundle/delimitmate
    git clone https://github.com/junegunn/goyo.vim ~/.vim/bundle/goyo.vim
    git clone https://github.com/scrooloose/nerdtree ~/.vim/bundle/nerdtree
    git clone https://github.com/reedes/vim-pencil ~/.vim/bundle/vim-pencil
    git clone https://github.com/tpope/vim-sensible ~/.vim/bundle/vim-sensible
    git clone https://github.com/tpope/vim-surround ~/.vim/bundle/vim-surround
    git clone https://github.com/christoomey/vim-tmux-navigator ~/.vim/bundle/vim-tmux-navigator
    git clone https://github.com/valloric/youcompleteme ~/.vim/bundle/youcompleteme

    cd ~/.vim/bundle/youcompleteme
    git submodule update --init --recursive
    ./install.py
fi

if ask "Install Better Defaults for Emacs?" N; then
    git clone https://github.com/technomancy/better-defaults.git ~/.emacs.d/better-defaults
fi

if ask "Install base16-shell?" N; then
    git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
fi

if ask "Install Termite? (Ubuntu Only)" N; then
    $HOME/dotfiles/scripts/install/termite.sh
fi
