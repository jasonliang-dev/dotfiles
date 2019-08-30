#!/usr/bin/env sh

. $(dirname "$0")/shared

echo 1 > $BAR_TOGGLE

# copy configs
cp ~/.cache/wal/colors-themerc ~/.themes/dark/openbox-3/themerc
cp ~/.cache/wal/colors-tint2rc ~/.config/tint2/tint2rc
cp ~/.cache/wal/colors-dunstrc ~/.config/dunst/dunstrc
cp ~/.cache/wal/colors-polybar ~/.config/polybar/config

# restart xmonad. xmobar also restarts
[ $(pgrep xmonad) ] && xmonad --restart

# reconfigure openbox. shows new theme
[ $(pgrep openbox) ] && openbox --reconfigure

# restart tint2
[ $(pgrep tint2) ] && killall -SIGUSR1 tint2

# run feh with new background
~/scripts/feh.sh

# set lockscreen background
betterlockscreen -u "$(< "${HOME}/.cache/wal/wal")"

# restart dunst
killall dunst
# dunst starts automatically with `notify-send` but not with spotify
# notifications. start dunst manually.
dunst &
