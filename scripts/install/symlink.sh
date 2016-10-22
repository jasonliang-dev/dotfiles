#!/bin/bash

source $HOME/dotfiles/scripts/ask.sh

mkdir -p $HOME/.config

echo "Created some symlinks...\n"
ln -sf $HOME/dotfiles/.alias                  $HOME/.alias
ln -sf $HOME/dotfiles/.bashrc                 $HOME/.bashrc
ln -sf $HOME/dotfiles/.config/compton.conf    $HOME/.config/compton.conf
ln -sf $HOME/dotfiles/.config/gtk-3.0/gtk.css $HOME/.config/gtk-3.0/gtk.css
ln -sf $HOME/dotfiles/.config/termite         $HOME/.config/termite
ln -sf $HOME/dotfiles/scripts                 $HOME/scripts
ln -sf $HOME/dotfiles/startpage               $HOME/startpage
ln -sf $HOME/dotfiles/.tmux.conf              $HOME/.tmux.conf
ln -sf $HOME/dotfiles/.vimrc                  $HOME/.vimrc
ln -sf $HOME/dotfiles/.Xresources             $HOME/.Xresources
ln -sf $HOME/dotfiles/.zprofile               $HOME/.zprofile
ln -sf $HOME/dotfiles/.zshrc                  $HOME/.zshrc

if ask "Create symlink for awesome?" N; then
    ln -sfv $HOME/dotfiles/.config/awesome $HOME/.config/awesome
fi

if ask "Create symlink for Openbox?" N; then
    ln -sfv $HOME/dotfiles/.config/openbox $HOME/.config/openbox
fi

if ask "Create symlink for i3?" N; then
    ln -sfv $HOME/dotfiles/.i3 $HOME/.i3
fi

if ask "Create symlink for everythine else?" N; then
    mkdir -p $HOME/.config/dunst
    mkdir -p $HOME/.config/gtk-3.0
    mkdir -p $HOME/.weechat
    mkdir -p $HOME/.mpd

    ln -sfv $HOME/dotfiles/.config/dunst/dunstrc $HOME/.config/dunst/dunstrc
    ln -sfv $HOME/dotfiles/.config/tint2         $HOME/.config/tint2
    ln -sfv $HOME/dotfiles/.mpd/mpd.conf         $HOME/.mpd/mpd.conf
    ln -sfv $HOME/dotfiles/.ncmpcpp              $HOME/.ncmpcpp
    ln -sfv $HOME/dotfiles/.themes               $HOME/.themes
    ln -sfv $HOME/dotfiles/.weechat/weechat.conf $HOME/.weechat/weechat.conf
fi
