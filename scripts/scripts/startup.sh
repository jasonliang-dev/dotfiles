#!/usr/bin/env sh

if pgrep i3 || pgrep bspwm > /dev/null; then
    ~/scripts/polybar &
elif pgrep openbox > /dev/null; then
    tint2 &
else
    exit 1
fi

#compton &
dunst &
~/scripts/feh.sh &
setxkbmap -option "caps:escape" &
setxkbmap -option "compose:ralt" &
xset m 0 0 &
xset r rate 270 27 &
