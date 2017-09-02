#!/usr/bin/env sh

# add paper project repo
sudo add-apt-repository ppa:snwh/pulp
# add papirus (material icon theme)
sudo add-apt-repository ppa:papirus/papirus
# add adapta (gtk theme)
sudo add-apt-repository ppa:tista/adapta

sudo apt update

sudo apt install\
     adapta-gtk-theme\
     arc-theme\
     compton\
     dunst\
     feh\
     lxappearance\
     openbox\
     paper-icon-theme\
     papirus-icon-theme\
     thunar\
     tint2\
     vim-gtk\
