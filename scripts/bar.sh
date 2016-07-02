#!/bin/bash

battery () {
    battery="$(</sys/class/power_supply/BAT1/capacity)"
    charging="$(</sys/class/power_supply/BAT1/status)"

    case "$battery" in
        [0-9]|10)
            battery="${battery}%  "
        ;;

        1[0-9]|2[0-5])
            battery="${battery}%  "
        ;;

        2[6-9]|3[0-9]|4[0-9]|50)
            battery="${battery}%  "
        ;;

        5[1-9]|6[0-9]|7[0-5])
            battery="${battery}%  "
        ;;

        7[6-9]|8[0-9]|9[0-9]|100)
            battery="${battery}%  "
        ;;
    esac

    [ "$charging" == "Charging" ] && \
        battery="  $battery"

    printf "%s" "$battery"
}

while :; do
        echo "        $(date "+%I:%M%P") %{r}$(battery)        %{r}"
            sleep 2s
done |

lemonbar -d -b -g "169x47+1165+16" -f "roboto-10" -f "fontawesome-12" -B "#333333" -F "#E0E0E0"
