#!/usr/bin/env sh

wp_dir="$HOME/Dropbox/wallpapers/"

case "$1" in
	"anime") wp_dir="${wp_dir}anime/" ;;
	"comfy") wp_dir="${wp_dir}comfy/" ;;
	"flat") wp_dir="${wp_dir}flat/" ;;
	"mnt") wp_dir="${wp_dir}mnt/" ;;
	"moe") wp_dir="${wp_dir}moe/" ;;
	"simple") wp_dir="${wp_dir}simple/" ;;
	"tile")
      feh --randomize --bg-tile "${wp_dir}tile/"
      exit 0
      ;;
esac

feh --randomize --bg-fill ${wp_dir}*
