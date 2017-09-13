#!/usr/bin/env sh

# add paper project repo
sudo add-apt-repository ppa:snwh/pulp
# add papirus (material icon theme)
sudo add-apt-repository ppa:papirus/papirus
# add adapta (gtk theme)
sudo add-apt-repository ppa:tista/adapta

sudo apt update

# install some things
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
     rofi\
     thunar\
     tint2\
     vim-gtk

# install some more things
sudo apt install\
	 emacs\
	 texlive-full\
	 python-pygments
