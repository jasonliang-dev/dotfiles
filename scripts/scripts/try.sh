#!/usr/bin/env sh

COMMANDS=("$@")

for COMMAND in "$COMMANDS"; do
    if [ ! -z "$(command -v $COMMAND)" ]; then
        $COMMAND
    fi
done
