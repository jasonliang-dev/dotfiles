#!/usr/bin/env sh

if [ -z "$@" ]; then
  ARGS="$HOME"
else
  ARGS="$@"
fi

xdg-open "$ARGS"
