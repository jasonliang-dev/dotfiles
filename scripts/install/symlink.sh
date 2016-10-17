#!/bin/bash

f="-sfv"

source $HOME/dotfiles/scripts/ask.sh

mkdir -p $HOME/.config

if ask "Create symlink for awesome?" N; then
    ln $f $HOME/dotfiles/.config/awesome $HOME/.config/awesome
fi
if ask "Create symlink for Openbox?" N; then
    ln $f $HOME/dotfiles/.config/openbox $HOME/.config/openbox
fi
if ask "Create symlink for i3?" N; then
    ln $f $HOME/dotfiles/.i3 $HOME/.i3
fi

if ask "Create symlink for .bashrc?" Y; then
    ln $f $HOME/dotfiles/.bashrc $HOME/.bashrc
fi
if ask "Create symlink for .zshrc?" Y; then
    ln $f $HOME/dotfiles/.zshrc $HOME/.zshrc
fi

if ask "Create symlink for everythine else?" Y; then
    mkdir -p $HOME/.config/dunst
    mkdir -p $HOME/.config/gtk-3.0
    mkdir -p $HOME/.gimp
    mkdir -p $HOME/.weechat
    mkdir -p $HOME/.mpd

    ln -f $HOME/dotfiles/.alias                   $HOME/.alias
    ln $f $HOME/dotfiles/.config/dunst/dunstrc    $HOME/.config/dunst/dunstrc
    ln $f $HOME/dotfiles/.config/tint2            $HOME/.config/tint2
    ln $f $HOME/dotfiles/.config/compton.conf     $HOME/.config/compton.conf
    ln $f $HOME/dotfiles/.config/gtk-3.0/gtk.css  $HOME/.config/gtk-3.0/gtk.css
    ln $f $HOME/dotfiles/.config/termite          $HOME/.config/termite
    ln $f $HOME/dotfiles/.gimp/menurc             $HOME/.gimp/menurc
    ln $f $HOME/dotfiles/.mpd/mpd.conf            $HOME/.mpd/mpd.conf
    ln $f $HOME/dotfiles/.ncmpcpp                 $HOME/.ncmpcpp
    ln $f $HOME/dotfiles/.themes                  $HOME/.themes
    ln $f $HOME/dotfiles/.weechat/weechat.conf    $HOME/.weechat/weechat.conf
    ln $f $HOME/dotfiles/scripts                  $HOME/scripts
    ln $f $HOME/dotfiles/startpage                $HOME/startpage
    ln $f $HOME/dotfiles/.tmux.conf               $HOME/.tmux.conf
    ln $f $HOME/dotfiles/.vimrc                   $HOME/.vimrc
    ln $f $HOME/dotfiles/.Xresources              $HOME/.Xresources
    ln $f $HOME/dotfiles/.zprofile                $HOME/.zprofile
fi
