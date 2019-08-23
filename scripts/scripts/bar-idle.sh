#!/usr/bin/env sh

. $(dirname "$0")/shared

IDLE_THRESHOLD=${1:-5000}

while sleep .1; do
    # bar set to always be showned or hidden.
    if [ -f $BAR_TOGGLE ] && [ $(cat $BAR_TOGGLE) -ne 0 ]; then
        continue
    fi

    IDLE_TIME=$(xprintidle)

    if [ $IDLE_TIME -gt $IDLE_THRESHOLD ]; then
        if [ ! -f $BAR_SHOWN ]; then
            touch $BAR_SHOWN
            polybar-msg cmd show > /dev/null
        fi
    elif [ -f $BAR_SHOWN ]; then
        rm $BAR_SHOWN
        polybar-msg cmd hide > /dev/null
    fi
done
