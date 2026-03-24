#!/usr/bin/bash
# Symlinks all the config files to the proper locations on home folder
# ADD FILES HERE: (RELATIVE FOR $HOME/dotfiles, NO ABSOLUTE PATH)

files=(
  ".emacs.d/lisp"
  ".emacs.d/snippets"
  ".gitconfig"
  ".emacs.d/init.el"
  "emacs_guide.txt"
  ".bashrc"
  "startup.sh"
  ".zshrc"
  ".p10k.zsh"
  ".nvidia-settings-rc"
  ".config/nvim/init.lua"
  ".config/kwinrc"
  ".gtkrc-2.0"
  ".config/gf2_config.ini"
  ".config/dolphinrc"
  ".local/bin/setup_cc.sh"
  ".local/bin/setup_cc"
  ".tmux.conf"
  ".config/alacritty"
  ".config/btop"
)

# Is verbose checking
isVerbose=false
if [[ "$1" == "-v" ]] || [[ "$1" == "--verbose" ]]; then
  isVerbose=true
fi

# Iterate files
for file in "${files[@]}"
do
  if [[ ! -e $HOME/$file ]] && [[ ! -L $HOME/$file ]]; then
    $isVerbose && echo "Symlinking $file..." || true
    ln -s $HOME/dotfiles/$file $HOME/$file && echo "Successfully symlinked $file"
  else
    $isVerbose && echo "Skipping: $file" || true
  fi
done
