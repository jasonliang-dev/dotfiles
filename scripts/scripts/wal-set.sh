#!/usr/bin/env sh

. $(dirname "$0")/shared

echo 1 > $BAR_TOGGLE

# copy configs
cp ~/.cache/wal/colors-themerc ~/.themes/dark/openbox-3/themerc
cp ~/.cache/wal/colors-tint2rc ~/.config/tint2/tint2rc
cp ~/.cache/wal/colors-dunstrc ~/.config/dunst/dunstrc
cp ~/.cache/wal/colors-polybar ~/.config/polybar/config

# reconfigure openbox. shows new theme
openbox --reconfigure

# run feh with new background
~/scripts/feh.sh

# set lockscreen background
betterlockscreen -u "$(< "${HOME}/.cache/wal/wal")"

# restart dunst
killall dunst
# dunst starts automatically with `notify-send` but not with spotify
# notifications. start dunst manually.
dunst &
