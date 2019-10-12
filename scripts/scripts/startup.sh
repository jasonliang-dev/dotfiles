#!/usr/bin/env sh

# remap keys
# caps to escape, right alt to compose
setxkbmap -option caps:escape
setxkbmap -option compose:ralt
# no mouse acceleration
xset m 0 0 &
# set keyboard repeat rate
xset r rate 200 50 &
# I want to move the mouse and hold down keys at the same time
xinput set-prop 12 "libinput Disable While Typing Enabled" 0 &
# set bindings
sxhkd &

# set background
~/scripts/feh.sh &
# use compton
compton &

# use network manager applet
nm-applet &
# I like my eyes and I like sleep
redshift-gtk &

# lock screen after 10 minutes
xautolock -time 10 -corners 000- -locker $HOME/scripts/lock.sh &

# optional stuff
while [ "$#" -gt 0 ]
do
    case $1 in
        "--dunst")
            dunst &
            ;;
        "--polybar")
            shift
            ~/scripts/polybar.sh $1 &
            ;;
        "--tint2")
            tint2 &
            ;;
        "--fix-cursor")
           xsetroot -cursor_name left_ptr &
           ;;
    esac

    shift
done
