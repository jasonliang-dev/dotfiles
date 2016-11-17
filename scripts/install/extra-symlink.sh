#!/bin/bash

source $HOME/dotfiles/scripts/ask.sh

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
    mkdir -p $HOME/.mpd
    mkdir -p $HOME/.weechat

    ln -sfv $HOME/dotfiles/.config/dunst/dunstrc $HOME/.config/dunst/dunstrc
    ln -sfv $HOME/dotfiles/.config/tint2         $HOME/.config/tint2
    ln -sfv $HOME/dotfiles/.mpd/mpd.conf         $HOME/.mpd/mpd.conf
    ln -sfv $HOME/dotfiles/.ncmpcpp              $HOME/.ncmpcpp
    ln -sfv $HOME/dotfiles/.weechat/weechat.conf $HOME/.weechat/weechat.conf
fi
