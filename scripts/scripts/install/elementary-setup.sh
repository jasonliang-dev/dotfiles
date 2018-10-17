#!/bin/sh

# double click to open files
gsettings set io.elementary.files.preferences single-click false

# use fixed number of workspaces
gsettings set org.pantheon.desktop.gala.behavior dynamic-workspaces false

# set number of workspaces
gsettings set org.gnome.desktop.wm.preferences num-workspaces 4

