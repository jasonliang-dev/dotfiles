#!/usr/bin/env sh

. $(dirname "$0")/shared

# set caps lock to escape
setxkbmap -option caps:escape &
# no mouse acceleration
xset m 0 0 &
# set keyboard repeat rate
xset r rate 200 50 &
# I want to move the mouse and hold down keys at the same time
xinput set-prop 12 "libinput Disable While Typing Enabled" 0 &
# set background
~/scripts/feh.sh &
# use compton
compton &
# use network manager applet
nm-applet &
# lock screen after 10 minutes
xautolock -time 10 -locker $HOME/scripts/lock.sh &

while [ "$#" -gt 0 ]
do
    case $1 in
        "--dunst")
            dunst &
            ;;
        "--polybar")
            ~/scripts/polybar.sh $BAR_NAME &

            if "$BAR_TOGGLE_ENABLED"; then
                echo 1 > $BAR_TOGGLE
                ~/scripts/bar-idle.sh &
            fi
            ;;
        "--tint2")
            tint2 &
            ;;
    esac

    shift
done
