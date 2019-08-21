#!/usr/bin/env sh

TOGGLE=/tmp/.lia-scrlk

if [ ! -e $TOGGLE ]; then
    touch $TOGGLE
    xset led named "Scroll Lock"
else
    rm $TOGGLE
    xset -led named "Scroll Lock"
fi
