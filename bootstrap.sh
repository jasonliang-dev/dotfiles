#!/usr/bin/env bash

bootstrap_list=(
    .bash_profile
    .bashrc
    .emacs.d
    .tmux.conf
    .xinitrc
    scripts
)

if [[ $1 == "clean" ]]
then
    for i in "${bootstrap_list[@]}"
    do
        echo removing ~/$i
        rm -r ~/$i
    done
else
    dotfiles_dir="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

    for i in "${bootstrap_list[@]}"
    do
        echo creating ~/$i
        ln -s $dotfiles_dir/$i ~/$i
    done
fi

