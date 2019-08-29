#!/usr/bin/env sh

amixer sget Master | awk -F "[][]" '/%/ { print $2; exit }'
