#!/usr/bin/env sh

(termite \
     || urxvt \
     || xfce4-terminal \
     || lxterminal \
     || gnome-terminal \
     || io.elementary.terminal) 2> /dev/null
