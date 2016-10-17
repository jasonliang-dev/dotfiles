#!/bin/bash

# Install
mkdir -p ~/.vim/autoload ~/.vim/bundle
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

# Install plugins
git clone https://github.com/chriskempson/base16-vim ~/.vim/bundle/base16-vim
git clone https://github.com/ctrlpvim/ctrlp.vim.git ~/.vim/bundle/ctrlp.vim
git clone https://github.com/raimondi/delimitmate ~/.vim/bundle/delimitmate
git clone https://github.com/scrooloose/nerdtree ~/.vim/bundle/nerdtree
git clone https://github.com/tpope/vim-sensible ~/.vim/bundle/vim-sensible
git clone https://github.com/tpope/vim-surround ~/.vim/bundle/vim-surround
git clone https://github.com/christoomey/vim-tmux-navigator ~/.vim/bundle/vim-tmux-navigator
git clone https://github.com/valloric/youcompleteme ~/.vim/bundle/youcompleteme

# YouCompleteMe
cd ~/.vim/bundle/youcompleteme
git submodule update --init --recursive
./install.py
