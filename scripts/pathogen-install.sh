#!/bin/bash

# Install
mkdir -p ~/.vim/autoload ~/.vim/bundle
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

# Install plugins if script is present
cd ~/.vim/bundle/
~/scripts/vim-plugins.sh

# YouCompleteMe
cd ~/.vim/bundle/youcompleteme
git submodule update --init --recursive
./install.py
