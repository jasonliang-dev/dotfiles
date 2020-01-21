# autostart X if on arch
if [[ -f "/etc/arch-release" ]] && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
    exit
fi

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

export ANDROID_HOME=$HOME/Android/Sdk
PATH=$PATH:$ANDROID_HOME/tools
PATH=$PATH:$ANDROID_HOME/emulator
PATH=$PATH:$ANDROID_HOME/platform-tools

PATH=$PATH:$HOME/.config/composer/vendor/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.cabal/bin
PATH=$PATH:$HOME/dotfiles/bin
PATH=$PATH:$HOME/.swift/usr/bin

export PATH
