#!/usr/bin/env sh

wp_dir="$HOME/Dropbox/wallpapers/"

case "$1" in
	"anime") wp_dir="${wp_dir}anime/" ;;
	"comfy") wp_dir="${wp_dir}comfy/" ;;
	"flat") wp_dir="${wp_dir}flat/" ;;
	"mnt") wp_dir="${wp_dir}mnt/" ;;
	"moe") wp_dir="${wp_dir}moe/" ;;
	"simple") wp_dir="${wp_dir}simple/" ;;
esac

feh --randomize --bg-fill ${wp_dir}*
