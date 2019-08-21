#!/usr/bin/env sh

. $(dirname "$0")/shared

echo 1 > $BAR_TOGGLE

cp ~/.cache/wal/colors-themerc ~/.themes/dark/openbox-3/themerc
cp ~/.cache/wal/colors-tint2rc ~/.config/tint2/tint2rc
cp ~/.cache/wal/colors-dunstrc ~/.config/dunst/dunstrc
cp ~/.cache/wal/colors-polybar ~/.config/polybar/config

openbox --reconfigure
killall dunst

~/scripts/feh.sh
dunst &
