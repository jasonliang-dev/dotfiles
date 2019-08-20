#!/usr/bin/env sh

COMMANDS=($@)

for COMMAND in $COMMANDS
do
    if [ $(command -v $COMMAND) ]; then
        $COMMAND
        exit 0
    fi
done

exit 1
