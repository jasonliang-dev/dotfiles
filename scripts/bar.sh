#!/bin/bash

# I'm not very good at this...
desktop() {
    ws=`xprop -root _NET_CURRENT_DESKTOP | awk '{print $3}'`
    case $ws in
        0)  echo web
            ;;
        1)  echo term
            ;;
        2)  echo music
            ;;
        *)  echo etc
            ;;
    esac
}
window() {
    xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) WM_CLASS | 
    cut -d '"' -f2
}
date0() {
    date "+%A, %B %d"
}
date1() {
    date "+%I:%M %P"
}
power() {
    /home/jason/scripts/i3/battery
}
music() {
    /home/jason/scripts/i3/mediaplayer
}
volume() {
    /home/jason/scripts/i3/volume
}

# Center bar
# Date and time
while true; do
    echo "%{c}  $(date0)          $(date1)"
    sleep 10
done |
lemonbar -d -b -g "598x28+384+0" -f "sourcesanspro-9" -f "fontawesome-10" -o 0 -B "#2B303B" -F "#EFF1F5" &

# Left bar
# Workspace
while true; do
    echo "%{c}  $(desktop)"
    sleep 1
done |
lemonbar -d -b -g "128x28+0+0" -f "sourcesanspro-9" -f "fontawesome-10" -o 0  -B "#c0c5ce" -F "#2b303b" &

# Window title
while true; do
    echo "%{c}  $(window)"
    sleep 1
done |
lemonbar -d -b -g "256x28+128+0" -f "sourcesanspro-9" -f "fontawesome-10" -o 0  -B "#4F5B66" -F "#EFF1F5" &

sleep .1

# Right bar
# Volume and battery
while true; do
    echo "%{c}$(volume)        $(power)"
    sleep 1
done |
lemonbar -d -b -g "128x28+1238+0" -f "sourcesanspro-9" -f "fontawesome-10" -o 0 -B "#c0c5ce" -F "#2b303b" &

# Music
while true; do
    echo "%{c}  $(music)"
    sleep 1
done |
lemonbar -d -b -g "256x28+982+0" -f "sourcesanspro-9" -f "fontawesome-10" -o 0  -B "#4F5B66" -F "#EFF1F5" &
