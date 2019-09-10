#!/usr/bin/env sh

if [ $(pgrep tint2 || pgrep polybar) ]; then
    exit
elif [ $(pgrep stalonetray) ]; then
    pkill stalonetray
    killall nm-applet
else
    nm-applet --no-agent 2>&1 >/dev/null &
    stalonetray --window-strut none 2>&1 >/dev/null &
fi
