#!/usr/bin/env sh

# add paper project repo
sudo add-apt-repository ppa:snwh/pulp
# add papirus (material icon theme)
sudo add-apt-repository ppa:papirus/papirus
# add adapta (gtk theme)
sudo add-apt-repository ppa:tista/adapta

sudo apt update

sudo apt install\
     arc-theme\
     papirus-icon-theme\
     adapta-gtk-theme\
     compton\
     feh\
     lxappearance\
     openbox\
     paper-icon-theme\
     thunar\
     tint2\
     vim-gtk\
