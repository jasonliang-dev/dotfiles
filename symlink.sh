#!/bin/bash

f="-sfv"
dot="$HOME/dotfiles"

ask() {
    # http://djm.me/ask
    while true; do
        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi
        # Ask the question - use /dev/tty in case stdin is redirected from somewhere else
        read -p "$1 [$prompt] " REPLY </dev/tty
        # Default?
        if [ -z "$REPLY" ]; then
            REPLY=$default
        fi
        # Check if the reply is valid
        case "$REPLY" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac
    done
}

mkdir -p $HOME/.config

if ask "Create symlink for awesome?" N; then
    ln $f $dot/.config/awesome $HOME/.config/awesome
fi
if ask "Create symlink for Openbox?" Y; then
    ln $f $dot/.config/openbox $HOME/.config/openbox
fi
if ask "Create symlink for i3?" N; then
    ln $f $dot/.i3 $HOME/.i3
fi

if ask "Create symlink for .bashrc?" Y; then
    ln $f $dot/.bashrc $HOME/.bashrc
fi
if ask "Create symlink for .zshrc?" Y; then
    ln $f $dot/.zshrc $HOME/.zshrc
fi

if ask "Create symlink for everythine else?"; then
    ln $f $dot/.config/dunst        $HOME/.config/dunst
    ln $f $dot/.config/mpd          $HOME/.config/mpd
    ln $f $dot/.config/tint2        $HOME/.config/tint2
    ln $f $dot/.config/compton.conf $HOME/.config/compton.conf
    ln $f $dot/.gimp                $HOME/.gimp
    ln $f $dot/.ncmpcpp             $HOME/.ncmpcpp
    ln $f $dot/.themes              $HOME/.themes
    ln $f $dot/.weechat             $HOME/.weechat
    ln $f $dot/scripts              $HOME/scripts
    ln $f $dot/startpage            $HOME/startpage
    ln $f $dot/.tmux.conf           $HOME/.tmux.conf
    ln $f $dot/.vimrc               $HOME/.vimrc
    ln $f $dot/.Xresources          $HOME/.Xresources
fi
