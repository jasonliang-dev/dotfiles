#!/usr/bin/env sh

BACKLIGHT_DIR=${1:-acpi_video0}

ABSOLUTE_DIR=/sys/class/backlight/$BACKLIGHT_DIR

if [ -d $ABSOLUTE_DIR ]; then
    ACTUAL=$(cat $ABSOLUTE_DIR/actual_brightness)
    MAX_BRIGHTNESS=$(cat $ABSOLUTE_DIR/max_brightness)

    if [ ! -z $ACTUAL ] && [ ! -z $MAX_BRIGHTNESS ]; then
        echo $(( 100 * $ACTUAL / $MAX_BRIGHTNESS ))%
        exit 0
    fi
fi

exit 1
