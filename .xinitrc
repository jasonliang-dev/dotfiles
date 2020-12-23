while xsetroot -name " `date +'%b %d, %I:%M%P'` "
do
    sleep 1
done &

exec dwm
