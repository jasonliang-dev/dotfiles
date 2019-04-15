#!/usr/bin/env bash

source $HOME/.env

rofi \
    -show run \
    -fuzzy \
    -lines 5 \
    -eh 1 \
    -width 40 \
    -padding 80 \
    -bw 0 \
    -color-enabled \
    -color-window "#EE"$BASE00,"#00000000","#00000000" \
    -color-normal "#00000000","#FF"$BASE03,"#00000000","#00000000","#FF"$BASE04 \
    -font "Roboto Regular 20"
