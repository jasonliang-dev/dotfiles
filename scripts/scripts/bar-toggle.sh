#!/usr/bin/env sh

. $(dirname "$0")/shared

if [ ! -f $BAR_TOGGLE ]; then
    echo 1 > $BAR_TOGGLE
fi

# cycle 0 -> 1 -> 2 -> 0 -> 1 -> ...
i=$(( ($(cat $BAR_TOGGLE) + 1) % 3 ))
echo $i > $BAR_TOGGLE

case $(cat $BAR_TOGGLE) in
    0)
        # hide when active (moving mouse, typing).
        # show bar when inactive (look at bar-idle.sh).
        polybar-msg cmd hide > /dev/null
        notify-send "Bar auto show/hide"
        ;;
    1)
        # show bar
        polybar-msg cmd restart > /dev/null
        notify-send "Bar showned"
        ;;
    2)
        # hide bar
        polybar-msg cmd hide > /dev/null
        notify-send "Bar hidden"
        ;;
esac
