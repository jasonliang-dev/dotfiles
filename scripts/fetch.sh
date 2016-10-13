#!/bin/bash

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
cBlue=$'\e[34m'
cPurple=$'\e[35m'

distro=$(cat /etc/*-release | grep -oP 'PRETTY_NAME=\K".*"' | tr -d '"')
kernel=$(uname -rm)
uptime=$(uptime -p | sed 's/up //')
if [ -f /etc/debian_version ]; then
    numpkg=$(dpkg -l | wc -l)
else
    numpkg=$(pacman -Q | wc -l)
fi
shell=$(echo $SHELL)
resolution=$(xdpyinfo | grep 'dimensions' | awk '{print $2}')
processor=$(grep -oP 'model name\K.*' /proc/cpuinfo | sed -e 's/^[ \t]: *//' | uniq)

#Clear screen and print system info
clear
printf "$cBlue$USER$cDefault at $cPurple$(hostname)$cDefault

\e[1mDistro:\e[21m     $distro
\e[1mKernel:\e[21m     $kernel
\e[1mUptime:\e[21m     $uptime
\e[1mPackages:\e[21m   $numpkg
\e[1mShell:\e[21m      $shell
\e[1mResolution:\e[21m $resolution
\e[1mCPU:\e[21m        $processor

            $bRed    $bGreen    $bYellow    $bBlue    $bPurple    $bCyan    $bDefault

"
