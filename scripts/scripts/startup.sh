#!/usr/bin/env sh

setxkbmap -option caps:escape
xset m 0 0
xset r rate 270 27
xinput set-prop 12 "libinput Disable While Typing Enabled" 0
~/scripts/feh.sh tile

compton &
nm-applet &
# dunst &
# tint2 &
# ~/scripts/polybar.sh &
