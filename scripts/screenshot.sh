#!/usr/bin/env bash

mkdir -p ~/screenshots/

img=~/screenshots/$(date +'%F-%H%M').png

case $1 in
    "region")
        import $img
        ;;
    *)
        scrot $img
esac

xclip -selection clipboard -target image/png -i $img
