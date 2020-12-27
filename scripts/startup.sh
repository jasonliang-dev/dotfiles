#!/usr/bin/env sh

# caps -> escape, right alt -> compose
setxkbmap -option caps:escape
setxkbmap -option compose:ralt
# keyboard repeat rate
xset r rate 180 45

# no mouse acceleration
xset m 0 0
# move mouse cursor with trackpad and type keys at the same time
xinput set-prop 12 "libinput Disable While Typing Enabled" 0

feh --bg-fill --randomize ~/wallpapers/* &
picom --config ~/.picom.conf &
~/scripts/bar.sh &
