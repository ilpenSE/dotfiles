#!/bin/bash

main() {
  if [[ $UID != 0 ]]; then
    echo "You must run this with sudo."
    return 1
  fi

  # You must install sugar-candy theme: yay -S sddm-theme-sugar-candy-git --noconfirm

  sudo ln -s $(pwd)/sugar_candy_theme.conf /usr/share/sddm/themes/Sugar-Candy/theme.conf
  return 0
}

main
