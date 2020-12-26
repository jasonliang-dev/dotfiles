#!/usr/bin/env bash

width=1366
height=768
gappx=20
barheight=30

# base16 one dark
base00=#282c34
base01=#353b45
base02=#3e4451
base03=#545862
base04=#565c64
base05=#abb2bf
base06=#b6bdca
base07=#c8ccd4
base08=#e06c75
base09=#d19a66
base0A=#e5c07b
base0B=#98c379
base0C=#56b6c2
base0D=#61afef
base0E=#c678dd
base0F=#be5046

# xprop -id 31457283 | grep ^WM_NAME | sed 's/^.* = "\(.*\)"$/\1/'

dwm-msg --ignore-reply subscribe client_focus_change_event |
    jq '.client_focus_change_event.new_win_id' |
    while IFS=$'\n' read -r winid
    do
        echo xprop -id $winid | grep ^WM_NAME | sed 's/^.* = "\(.*\)"$/\1/'
    done |
    lemonbar -d -g "200x$barheight+$gappx+$gappx" \
             -f "Source Sans Pro:size=10:weight=200" \
             -o 0 -B $base01 -F $base05 &

dwm-msg --ignore-reply subscribe tag_change_event |
    jq --unbuffered '.tag_change_event.new_state | .selected, .occupied' |
    while IFS=$'\n' read -r selected && read -r occupied
    do
        out=""

        for tag in {0..8}
        do
            bit=$((1 << $tag))

            if [[ $(($selected & $bit)) -ne 0 ]]
            then
                out="${out}%{F$base01}%{B$base05} . "
            elif [[ $(($occupied & $bit)) -ne 0 ]]
            then
                out="${out}%{F$base05}%{B$base02} . "
            else
                out="${out}%{F$base05}%{B$base01} . "
            fi
        done

        echo "%{F-}%{B-}%{c}$out%{F-}%{B-}"
    done |
    lemonbar -d -g "300x$barheight+$(($width / 2 - 150))+$gappx" \
             -f "Roboto Mono:size=10:weight=200" \
             -o 0 -B $base01 -F $base05 &

while true
do
    echo "%{c}$(date +'%I:%M')"
    sleep 1
done |
    lemonbar -d -g "200x$barheight+$(($width - 200 - $gappx))+$gappx" \
             -f "Source Sans Pro:size=10:weight=200" \
             -o 0 -B $base01 -F $base05 &
