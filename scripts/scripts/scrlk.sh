#!/usr/bin/env sh

TOGGLE=$HOME/scripts/.scrlktoggle

if [ ! -e $TOGGLE ]; then
    touch $TOGGLE
    xset led named "Scroll Lock"
else
    rm $TOGGLE
    xset -led named "Scroll Lock"
fi
