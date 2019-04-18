#!/usr/bin/env sh

scrot -d $1 && notify-send "Snap!" "Screenshot taken!"
