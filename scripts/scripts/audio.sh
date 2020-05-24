#!/usr/bin/env sh

case "$1" in
    # thank you arch wiki
    # https://wiki.archlinux.org/index.php/PulseAudio#Keyboard_volume_control
    "up")
        pactl set-sink-mute @DEFAULT_SINK@ false
        pactl set-sink-volume @DEFAULT_SINK@ +2%
        ;;
    "down")
        pactl set-sink-mute @DEFAULT_SINK@ false
        pactl set-sink-volume @DEFAULT_SINK@ -2%
        ;;
    "mute") pactl set-sink-mute @DEFAULT_SINK@ toggle ;;

    "play")  ~/scripts/try.sh "cmus-remote -u" "mpc toggle" "playerctl play-pause" ;;
    "pause") ~/scripts/try.sh "cmus-remote -s" "mpc pause" "playerctl pause" ;;
    "stop")  ~/scripts/try.sh "cmus-remote -s" "mpc stop" "playerctl stop" ;;
    "next")  ~/scripts/try.sh "cmus-remote -n" "mpc next" "playerctl next" ;;
    "prev")  ~/scripts/try.sh "cmus-remote -r" "mpc prev" "playerctl previous" ;;
esac
