mkdir -p $HOME/.vim/bundle
cd $HOME/.vim/bundle
if [ -d "$DIRECTORY" ]; then
  git clone https://github.com/chriskempson/base16-vim
  git clone https://github.com/raimondi/delimitmate
  git clone https://github.com/tpope/vim-sensible
  git clone https://github.com/tpope/vim-surround
  git clone https://github.com/christoomey/vim-tmux-navigator
  git clone https://github.com/valloric/youcompleteme
fi
