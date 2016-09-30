#!/bin/bash

resetAll=$'\e[0m'
underline='\e[4m'

#Background color
bDefault=$'\e[49m'
bRed=$'\e[41m'
bGreen=$'\e[42m'
bYellow=$'\e[43m'
bBlue=$'\e[44m'
bPurple=$'\e[45m'
bCyan=$'\e[46m'

#Foreground color
cDefault=$'\e[39m'
cRed=$'\e[31m'
cGreen=$'\e[32m'
cYellow=$'\e[33m'
cBlue=$'\e[34m'
cPurple=$'\e[35m'
cCyan=$'\e[36m'

#Clear screen and print system info
clear
printf "$cBlue$USER$cDefault at $cPurple$(hostname)$cDefault
Distro:     $(cat /etc/*-release | grep -oP 'PRETTY_NAME=\K".*"' | tr -d '"')
Kernel:     $(uname -rmo)
Uptime:     $(uptime -p | sed 's/up //')
Packages:   $(pacman -Q | wc -l)
Shell:      $(echo $SHELL)
Resolution: $(xdpyinfo | grep 'dimensions' | awk '{print $2}')
CPU:        $(grep -oP 'model name\K.*' /proc/cpuinfo | sed -e 's/^[ \t]: *//' | uniq)	

            $bRed    $bGreen    $bYellow    $bBlue    $bPurple    $bCyan    $bDefault

"
