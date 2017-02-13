#!/usr/bin/env sh

case "$1" in
    "up")   amixer set Master 2%+ ;;
    "down") amixer set Master 2%- ;;
    "mute") amixer set Master toggle ;;

    "play") cmus-remote -u || mpc toggle || playerctl-play-pause ;;
    "stop") cmus-remote -s || mpc $1 || playerctl $1 ;;
    "next") cmus-remote -n || mpc $1 || playerctl $1 ;;
    "prev") cmus-remote -r || mpc $1 || playerctl previous ;;
esac
