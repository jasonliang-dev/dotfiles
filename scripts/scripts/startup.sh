#!/usr/bin/env sh

compton &
dunst &
~/scripts/feh.sh &
~/scripts/polybar.sh &
setxkbmap -option caps:escape &
xset m 0 0 &
xset r rate 270 27 &
