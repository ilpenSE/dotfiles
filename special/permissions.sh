#!/bin/bash

main() {
  if [[ $UID != 0 ]]; then
    echo "You must run this with sudo"
    return 1
  fi

  USER_HOME="/home/ilpen" # Home of current user

  # If you dont give permission these subdirectories and the file
  # The theme won't be loaded. These are giving permission to sddm user:
  sudo setfacl -m u:sddm:X $USER_HOME
  sudo setfacl -m u:sddm:X $USER_HOME/dotfiles
  sudo setfacl -m u:sddm:X $USER_HOME/dotfiles/special
  sudo setfacl -m u:sddm:r $USER_HOME/dotfiles/special/sugar_candy_theme.conf
}

main
