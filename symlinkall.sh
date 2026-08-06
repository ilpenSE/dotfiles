#!/usr/bin/bash
# Symlinks all the config files to the proper locations on home folder
# ADD FILES HERE: (RELATIVE FOR $HOME/dotfiles, NO ABSOLUTE PATH)

files=(
  ".emacs.d/lisp"
  ".emacs.d/snippets"
  ".emacs.d/init.el"
  ".emacs.d/custom.el"
  "emacs_guide.txt"
  ".gitconfig"
  ".bashrc"
  "startup.sh"
  ".zshrc"
  ".p10k.zsh"
  ".nvidia-settings-rc"
  ".config/nvim"
  ".config/flameshot/flameshot.ini"
  ".config/gf2_config.ini"
  ".config/kwinrc"
  ".local/bin/setup_cc"
  ".tmux.conf"
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
