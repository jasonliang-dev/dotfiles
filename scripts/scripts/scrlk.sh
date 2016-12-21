#!/bin/sh

TOGGLE=$HOME/scripts/.toggle

if [ ! -e $TOGGLE ]; then
    touch $TOGGLE
    xset led named "Scroll Lock"
else
    rm $TOGGLE
    xset -led named "Scroll Lock"
fi
