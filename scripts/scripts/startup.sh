#!/usr/bin/env bash

setxkbmap -option caps:escape
xset m 0 0
xset r rate 200 50
xinput set-prop 12 "libinput Disable While Typing Enabled" 0
~/scripts/feh.sh tile

compton &
nm-applet &

while [ "$#" -gt 0 ]
do
    if [[ $1 = "--dunst" ]]
    then
        dunst &
    elif [[ $1 == "--tint2" ]]
    then
        tint2 &
    elif [[ $1 == "--polybar" ]]
    then
        ~/scripts/polybar.sh &
    fi

    shift
done
