#!/usr/bin/env sh

# make directories if they don't already exist
mkdir -p ~/.themes/dark/openbox-3
mkdir -p ~/.config/tint2
mkdir -p ~/.config/dunst
mkdir -p ~/.config/polybar

# copy configs
ln -sf ~/.cache/wal/colors-themerc ~/.themes/dark/openbox-3/themerc
ln -sf ~/.cache/wal/colors-tint2rc ~/.config/tint2/tint2rc
ln -sf ~/.cache/wal/colors-dunstrc ~/.config/dunst/dunstrc
ln -sf ~/.cache/wal/colors-polybar ~/.config/polybar/config
ln -sf ~/.cache/wal/colors-xmobarrc ~/.xmobarrc

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
