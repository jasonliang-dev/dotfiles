#!/usr/bin/env bash

compton --opengl --vsync opengl &
feh --bg-fill /home/jason/Pictures/wallpaper.jpg &
sleep 1; nm-applet &
xset r rate 270 27 
xset m 0 0 
#setxkbmap -option caps:swapescape 
setxkbmap -option 'ctrl:nocaps' 
xcape -e 'Control_L=Escape' 
