#!/bin/bash
# Setup Cross-Compile utilities
# Built for x86-64 Arch Linux
# Assumes you have already installed llvm-mingw, osxcross and msvc-wine in .local

BASH_RED="\e[1;31m"
BASH_GOLD="\e[1;38;2;255;165;0m"
BASH_RST="\e[0m"

setup_cc_msvc_for() {
  case $1 in
    x64 | x86 | arm64 | aarch64 | x86_64 | x86-64)
      ;;
    *)
      echo -e "$BASH_RED[ERROR]$BASH_RSTUnsupported platform for MSVC."
      echo "Supported: x64, x86 and arm64/aarch64"
      echo -e "Use: $BASH_GOLD$0 x64$BASH_RST for x86-64 MSVC"
      ;;
  esac
  local bindir="$HOME/.local/msvc-wine/bin/$1"
  export PATH="$bindir:$PATH"
  source "$bindir/msvcenv.sh"
  echo "Current cl path: $(command -v cl)"
}

setup_cc_mingw() {
  export PATH="$HOME/.local/llvm-mingw/bin:$PATH"
}

setup_cc_osx() {
  export LD_LIBRARY_PATH=$HOME/.local/osxcross/lib:$LD_LIBRARY_PATH
  export PATH="$HOME/.local/osxcross/bin:$PATH"
}

setup_cc() {
  case $1 in
    linux)
      ;; # nothing to do
    msvc | windows)
      setup_cc_msvc_for $2
      ;;
    mingw)
      setup_cc_mingw
      ;;
    osx)
      setup_cc_osx
      ;;
    *)
      printf "$BASH_RED[ERROR]$BASH_RST Unsupported os/target.\n"
      printf "Usage:\n"
      printf "  $BASH_GOLD$0 <mingw|osx>$BASH_RST\n"
      printf "     MinGW and OSX targets already configures aarch64 and amd64\n"
      printf "  $BASH_GOLD$0 msvc <x86|x64|arm64>$BASH_RST\n"
      printf "     Seperated because they have same names\n"
      printf "     x64   -> MSVC x86-64\n"
      printf "     x86   -> MSVC x86\n"
      printf "     arm64 -> MSVC ARM64\n"
      ;;
  esac
}
