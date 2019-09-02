#!/usr/bin/env sh

[ $(pgrep stalonetray) ] &&
    pkill stalonetray ||
    stalonetray --window-strut none 2>&1 >/dev/null &
