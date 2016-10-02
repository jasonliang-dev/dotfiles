#!/bin/bash
scrot /tmp/screen.png
convert /tmp/screen.png -blur 0x10 /tmp/screen.png
[[ -f $HOME/Pictures/Icons/lock.png ]] && convert /tmp/screen.png $HOME/Pictures/Icons/lock.png -gravity center -composite -matte /tmp/screen.png
i3lock -u -i /tmp/screen.png
