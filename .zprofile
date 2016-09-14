if [ -z "$DISPLAY" ] && [ "$(fgconsole)" -eq 1 ]; then
      exec startx
fi
