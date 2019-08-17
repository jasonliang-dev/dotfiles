#!/usr/bin/env bash

cp ~/.cache/wal/colors-themerc ~/dotfiles/openbox/.themes/dark/openbox-3/themerc
cp ~/.cache/wal/colors-tint2rc ~/dotfiles/openbox/.config/tint2/tint2rc

openbox --reconfigure
killall -SIGUSR1 tint2
