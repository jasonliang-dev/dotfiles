#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root" 
   exit 1
fi

ip link set down wlp3s0
macchanger -e wlp3s0
ip link set up wlp3s0
systemctl restart netctl-auto@wlp3s0.service
