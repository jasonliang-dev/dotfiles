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
        3)  echo tweak
            ;;
        *)  echo etc
            ;;
    esac
}
window() {
    xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) WM_CLASS | 
    cut -d '"' -f2
}
barDate() {
    date "+%a, %B %d"
}
barTime() {
    date "+%I:%M %P"
}
power() {
    percent="$(</sys/class/power_supply/BAT1/capacity)"
    status="$(</sys/class/power_supply/BAT1/status)"

    if [ $status == "Discharging" ]; then
        if [ $percent -lt 20 ]; then
            percent="  $percent%"
        elif [ $percent -lt 40 ]; then
            percent="  $percent%"
        elif [ $percent -lt 60 ]; then
            percent="  $percent%"
        elif [ $percent -lt 80 ]; then
            percent="  $percent%"
        else
            percent="  $percent%"
        fi
    else
        percent="  $percent%"
    fi
    
    echo $percent
}
music() {
    pgrep spotify > /dev/null && \
        echo   $(/home/jason/scripts/i3/mediaplayer)
}
volume() {
    /home/jason/scripts/i3/volume 5 pulse
}

font="-f sourcesanspro-9 -f fontawesome-10"

# Center bar
# Music
while true; do
    echo "%{c}$(music)"
    sleep 1
done |
lemonbar $font -g "598x28+384+0" -B "#343D46" -F "#EFF1F5" &

# Left bar
# Workspace
while true; do
    echo "%{c}  $(desktop)"
    sleep 1
done |
lemonbar $font -g "128x28+0+0" -B "#C0C5CE" -F "#2B303B" &

# Window title
while true; do
    echo "%{c}  $(window)"
    sleep 1
done |
lemonbar $font -g "256x28+128+0" -B "#4F5B66" -F "#EFF1F5" &

# Right bar
# Volume and battery
while true; do
    echo "%{c}$(volume)        $(power)"
    sleep 1
done |
lemonbar $font -g "128x28+1238+0" -B "#C0C5CE" -F "#2B303B" &

# Date and time
while true; do
    echo "%{c}  $(barDate)          $(barTime)"
    sleep 10
done |
lemonbar $font -g "256x28+982+0" -B "#4F5B66" -F "#EFF1F5" &
