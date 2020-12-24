#!/usr/bin/env bash

bootstrap_list=(
    .bash_profile
    .bashrc
    .emacs.d
    .tmux.conf
    .xinitrc
    scripts
)

red='\033[0;31m'
cyan='\033[0;36m'
nocol='\033[0m'

if [[ $1 == "clean" ]]
then
    for i in "${bootstrap_list[@]}"
    do
        echo -e removing ~/$red$i$nocol
        rm -r ~/$i
    done
else
    dotfiles_dir="$( cd "$(dirname "$0")" ; pwd -P )"

    for i in "${bootstrap_list[@]}"
    do
        echo -e $dotfiles_dir/$cyan$i$nocol "->" ~/$cyan$i$nocol
        ln -sT $dotfiles_dir/$i ~/$i
    done
fi

