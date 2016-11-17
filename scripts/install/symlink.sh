#!/bin/bash

source $HOME/dotfiles/scripts/ask.sh

mkdir -p $HOME/.config
mkdir -p $HOME/.emacs.d
mkdir -p $HOME/.config/gtk-3.0

ln -sf $HOME/dotfiles/.Xresources             $HOME/.Xresources
ln -sf $HOME/dotfiles/.alias                  $HOME/.alias
ln -sf $HOME/dotfiles/.bashrc                 $HOME/.bashrc
ln -sf $HOME/dotfiles/.config/compton.conf    $HOME/.config/compton.conf
ln -sf $HOME/dotfiles/.config/gtk-3.0/gtk.css $HOME/.config/gtk-3.0/gtk.css
ln -sf $HOME/dotfiles/.config/termite         $HOME/.config/termite
ln -sf $HOME/dotfiles/.emacs.d/init.el        $HOME/.emacs.d/init.el
ln -sf $HOME/dotfiles/.gitconfig              $HOME/.gitconfig
ln -sf $HOME/dotfiles/.themes                 $HOME/.themes
ln -sf $HOME/dotfiles/.tmux.conf              $HOME/.tmux.conf
ln -sf $HOME/dotfiles/.vimrc                  $HOME/.vimrc
ln -sf $HOME/dotfiles/.zprofile               $HOME/.zprofile
ln -sf $HOME/dotfiles/.zshrc                  $HOME/.zshrc
ln -sf $HOME/dotfiles/scripts                 $HOME/scripts
ln -sf $HOME/dotfiles/startpage               $HOME/startpage
