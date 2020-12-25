#!/usr/bin/env bash

mkdir -p ~/screenshots/

img=~/screenshots/$(date +'%F-%H%M').png
import $img
xclip -selection clipboard -target image/png -i $img
