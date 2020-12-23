#!/usr/bin/env sh

# caps -> escape
setxkbmap -option caps:escape

# right alt -> compose
setxkbmap -option compose:ralt

# no mouse acceleration
xset m 0 0

# keyboard repeat rate
xset r rate 180 45

# move mouse cursor with trackpad and type keys at the same time
xinput set-prop 12 "libinput Disable While Typing Enabled" 0

compton &
