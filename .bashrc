# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# history things
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

shopt -s checkwinsize
shopt -s globstar # support for **

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
  xterm-color|*-256color) color_prompt=yes;;
esac

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
  color_prompt=yes
    else
  color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\[\033[0m\]> '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\n> '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
  xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
  *)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export LC_ALL="en_US.UTF-8"

export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export LESS_TERMCAP_mb=$'\e[01;31m'  # Blink (Red)
export LESS_TERMCAP_md=$'\e[01;34m'  # Bold/Headers (Blue)
export LESS_TERMCAP_me=$'\e[0m'     # Reset
export LESS_TERMCAP_so=$'\e[01;33m' # Standout/Prompt (Yellow)
export LESS_TERMCAP_se=$'\e[0m'     # Reset Standout
export LESS_TERMCAP_us=$'\e[04;32m' # Underline (Green)
export LESS_TERMCAP_ue=$'\e[0m'     # Reset Underline

alias ls='eza -al --color=always --group-directories-first --icons=always' # preferred listing
alias la='eza -a --color=always --group-directories-first --icons=always'  # all files and dirs
alias ll='eza -l --color=always --group-directories-first --icons=always'  # long format
alias lt='eza -aT --color=always --group-directories-first --icons=always' # tree listing
alias l.="eza -a | grep -e '^\.'"                                          # show only dotfiles
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
alias hw='hwinfo --short'
alias jctl='journalctl -p 3 -xb'
alias mirror='sudo cachyos-rate-mirrors'
alias grep='grep --color=auto'
alias tb="nc termbin.com 9999"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

alias c="clear"
alias just-shutdown="sudo shutdown now"
alias just-reboot="sudo reboot now"
alias clang-cl="clang --driver-mode=cl"
alias x86_64-w64-mingw32-clang="clang --target=x86_64-w64-mingw32"
alias biosfw="sudo systemctl reboot --firmware-setup"
alias wine="MESA_DEBUG=silent EGL_LOG_LEVEL=fatal wine"
export WINEPATH="/usr/x86_64-w64-mingw32/bin"

eval "$(ssh-agent -s)" > /dev/null 2>&1
ssh-add ~/.ssh/id_ed25519 > /dev/null 2>&1

# User-defined paths, will be added to PATH
USER_PATHS=(
  "$HOME/.local/share/ij-idea/bin"
  "$HOME/apache-maven-3.9.12/bin"
  "$HOME/.local/bin"
)

export MANPATH="/usr/local/man:$MANPATH"

# Python environment activate function
activate-venv() {
  local env="${1:-venv}"
  local act="$HOME/$env/bin/activate"
  [[ -f "$act" ]] || { echo "No such venv: $venv"; return 1; }
  echo "Activating python venv in home: $venv"
  source "$act"
}

# cd and ls with flags combinations
cdls() {
  cd "$@" && ls
}

cdll() {
  cd "$@" && ls -lh
}

cdla() {
  cd "$@" && ls -ah
}

cdlla() {
  cd "$@" && ls -lah
}

UZ() {
  # Extract file name and extension separately
  f_name="$(basename "$1" | awk -F. 'BEGIN{OFS="_"} {if ($(NF-1) == "tar") {ext = $(NF-1) "." $NF; NF-=2} else {ext = $NF; NF--}; print $0}')"
  f_ext="$(echo "$1" | awk -F. '{if ($(NF-1) == "tar") {print $(NF-1) "." $NF} else {print $NF}}')"

  # Determine the last or last two dots to perform the actions
  case "$f_ext" in
    "zip")
      echo "unzipping zip to $f_name"
      mkdir "$f_name"
      unzip "$1" -d "$f_name"
      ;;
    "tar.gz" | "tgz")
      echo "unzipping tar.gz to $f_name"
      mkdir "$f_name"
      tar -zxvf "$1" -C "$f_name" --strip-components 1
      ;;
    "tar")
      echo "unzipping tar to $f_name"
      mkdir "$f_name"
      tar -xvf "$1" -C "$f_name"
      ;;
    "tar.xz")
      echo "unzipping tar.xz to $f_name"
      mkdir "$f_name"
      tar -xf "$1" -C "$f_name"
      ;;
    "gz")
      echo "unzipping gz to $f_name"
      mkdir "$f_name"
      gunzip -c "$1" > "$f_name"
      ;;
    "7z")
      echo "unzipping 7z to $f_name"
      mkdir "$f_name"
      7z x "$1" -o"$f_name"
      ;;
    *)
      echo "unknown file type: $f_ext"
      ;;
  esac
}

# Local to Google Drive:
# rclone sync ~/Drive gdrive:
# Google Drive to Local:
# rclone sync gdrive: ~/Drive
alias mount-gdrive="rclone mount gdrive: ~/Drive --vfs-cache-mode writes --poll-interval 0"
alias docker-set-permits="sudo chown -R ilpen:ilpen /home/ilpen/docker_data"
alias psqlconn="psql \"postgresql://postgres:postgres@127.0.0.1:54322/postgres\""
alias psqlrunq="PGPASSWORD=postgres psql -h 127.0.0.1 -p 54322 -U postgres -d postgres -f"

PATH="$(IFS=:; echo "${USER_PATHS[*]}"):$PATH"

source setup_cc > /dev/null 2>&1

# Echoes ID of the distro
# For example: in arch linux it'll print "arch"
distro_name() {
  source /etc/os-release
  echo "$ID"
}

# Autoremove functions for arch linux.
# Equivalent to: sudo apt autoremove
pacman-autoremove() {
  if [[ "$(distro_name)" =~ ^(arch|cachyos) ]]; then
    orphans=$(pacman -Qdtq)
    [ -n "$orphans" ] && sudo pacman -Rns $orphans || echo "No orphan package."
  else
    echo "Your distro does not have pacman"
  fi
}

yay-autoremove() {
  if [[ $(which yay 2>&1 > /dev/null && echo "$?") == 0 ]]; then
    yay -Yc
  else
    echo "You don't have yay installed"
  fi
}

all-autoremove() {
  pacman-autoremove
  yay-autoremove
}
