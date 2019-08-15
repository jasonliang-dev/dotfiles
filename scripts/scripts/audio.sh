#!/usr/bin/env sh

case "$1" in
    "up")   amixer set Master 2%+ ;;
    "down") amixer set Master 2%- ;;
    "mute") amixer set Master toggle ;;

    "play") cmus-remote -u || mpc toggle || playerctl play-pause ;;
    "stop") cmus-remote -s || mpc stop   || playerctl stop ;;
    "next") cmus-remote -n || mpc next   || playerctl next ;;
    "prev") cmus-remote -r || mpc prev   || playerctl previous ;;
esac
