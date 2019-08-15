#!/usr/bin/env sh

case "$1" in
    "up")   xbacklight +5 ;;
    "down") xbacklight -5 ;;
esac
