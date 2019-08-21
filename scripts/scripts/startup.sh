#!/usr/bin/env sh

. $(dirname "$0")/shared

setxkbmap -option caps:escape &
xset m 0 0 &
xset r rate 200 50 &
xinput set-prop 12 "libinput Disable While Typing Enabled" 0 &
~/scripts/feh.sh &

compton &
nm-applet &
xautolock -time 10 -locker $HOME/scripts/lock.sh &

while [ "$#" -gt 0 ]
do
    case $1 in
        "--dunst")
            dunst &
            ;;
        "--tint2")
            tint2 &
            ;;
        "--polybar")
            echo 1 > $BAR_TOGGLE
            ~/scripts/polybar.sh &
            ~/scripts/bar-idle.sh &
            ;;
    esac

    shift
done
