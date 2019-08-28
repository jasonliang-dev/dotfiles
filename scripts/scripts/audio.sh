#!/usr/bin/env sh

case "$1" in
    "up")   amixer set Master 2%+ ;;
    "down") amixer set Master 2%- ;;
    "mute") amixer set Master toggle ;;

    "play") ~/scripts/try.sh "cmus-remote -u" "mpc toggle" "playerctl play-pause" ;;
    "stop") ~/scripts/try.sh "cmus-remote -s" "mpc stop" "playerctl stop" ;;
    "next") ~/scripts/try.sh "cmus-remote -n" "mpc next" "playerctl next" ;;
    "prev") ~/scripts/try.sh "cmus-remote -r" "mpc prev" "playerctl previous" ;;
esac
