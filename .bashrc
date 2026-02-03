# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

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

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
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
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

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

alias la="ls -ah"
alias ll="ls -lh"
alias lla="ls -lah"
alias c="clear"

eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

clear

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Python environment activate function
activate-env() {
  local env="${1:-venv}"
  local act="$HOME/$env/bin/activate"

  [[ -f "$act" ]] || { echo "env yok: $env"; return 1; }

  echo "Activating python environment in home: $env"
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

# quick built-in password feeder into keepassxc
kp() {
  local db="${1:-$HOME/ŞİFRELER.kdbx}"
  [[ -f "$db" ]] || { echo "Böyle bir dosya yok: $db"; return 1; }

  read -s -p "Şifre (boş bırak = GUI): " pw
  echo

  if [[ -z "$pw" ]]; then
    keepassxc "$db" &
  else
    printf '%s' "$pw" | keepassxc --pw-stdin "$db" &
  fi
}

# "cmake" command for windows x86_64 environment
# it is not cmake --build, just the initializer
cmakewin() {
    cmake \
      -DCMAKE_SYSTEM_NAME=Windows \
      -DCMAKE_C_COMPILER=x86_64-w64-mingw32-gcc \
      -DCMAKE_CXX_COMPILER=x86_64-w64-mingw32-g++ \
      "$@"
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

export PATH="$PATH:$HOME/Qt/6.10.1/gcc_64/bin" # Qt Build Tools
export PATH="$PATH:$HOME/Downloads/idea-IU-253.29346.138/bin" # IntelliJ IDEA
export PATH="$PATH:$HOME/apache-maven-3.9.12/bin" # Apache Maven
export PATH="$PATH:$HOME/.local/bin"

. "$HOME/.cargo/env"
