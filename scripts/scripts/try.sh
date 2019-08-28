#!/usr/bin/env sh

for COMMAND in "$@"; do
    PROGRAM=$(echo "$COMMAND" | sed "s/ .*//")

    if [ $(command -v "$PROGRAM") ]; then
        $COMMAND
        exit $?
    fi
done

exit 1
