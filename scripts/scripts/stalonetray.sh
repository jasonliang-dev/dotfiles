#!/usr/bin/env sh

if [ $(pgrep stalonetray) ]; then
    pkill stalonetray
    killall nm-applet
else
    nm-applet --no-agent 2>&1 >/dev/null &
    stalonetray --window-strut none 2>&1 >/dev/null &
fi
