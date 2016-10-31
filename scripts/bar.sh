#!/bin/bash

# I'm not very good at this...
S() {
    echo "            "
}
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
    cut -d '"' -f 2
}
barDate() {
    date "+%a, %B %d"
}
barTime() {
    date "+%I:%M %P"
}
power() {
    percent=$(</sys/class/power_supply/BAT1/capacity)
    status=$(</sys/class/power_supply/BAT1/status)

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

while true; do
    echo "%{l}%{F#2B303B}%{B#C0C5CE}$(S)  $(desktop)$(S)%{F#EFF1F5}%{B#4F5B66}\
              $(window)$(S)%{B#343D46}%{F#EFF1F5}\
            %{c}$(music)\
            %{r}%{F#EFF1F5}%{B#4F5B66}$(S)  $(barDate)$(S)  $(barTime)$(S)%{F#2B303B}%{B#C0C5CE}\
            $(volume)$(S)$(power)$(S)%{B#343D46}"
    sleep 1
done |
lemonbar $font -g "1366x28+0+0" &
