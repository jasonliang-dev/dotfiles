#!/usr/bin/env sh

AMIX_OUT=$(amixer get Master)

if echo "$AMIX_OUT" | grep "\[off\]" > /dev/null 2>&1; then
    echo Muted
else
    echo "$AMIX_OUT" | awk -F "[][]" '/%/ { print $2; exit }'
fi


