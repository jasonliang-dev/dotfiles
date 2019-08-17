#!/usr/bin/env bash

cp ~/.cache/wal/colors-themerc ~/.themes/dark/openbox-3/themerc
cp ~/.cache/wal/colors-tint2rc ~/.config/tint2/tint2rc
cp ~/.cache/wal/colors-dunstrc ~/.config/dunst/dunstrc

openbox --reconfigure
killall -SIGUSR1 tint2
killall dunst

~/scripts/feh.sh
dunst &
