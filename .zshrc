if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="/usr/share/oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"
export ZSH_COMPDUMP="${XDG_CACHE_HOME:-$HOME/.cache}/zcompdump"

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
ENABLE_CORRECTION="true"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# PATH entries
local -a USER_PATHS=(
  "$HOME/.local/share/ij-idea/bin"
  "$HOME/apache-maven-3.9.12/bin"
  "$HOME/.local/bin"
)

export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG="en_US.UTF-8"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
export ARCHFLAGS="-arch $(uname -m)"
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Stolen from fish
export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export LESS_TERMCAP_mb=$'\e[01;31m'  # Blink (Red)
export LESS_TERMCAP_md=$'\e[01;34m'  # Bold/Headers (Blue)
export LESS_TERMCAP_me=$'\e[0m'     # Reset
export LESS_TERMCAP_so=$'\e[01;33m' # Standout/Prompt (Yellow)
export LESS_TERMCAP_se=$'\e[0m'     # Reset Standout
export LESS_TERMCAP_us=$'\e[04;32m' # Underline (Green)
export LESS_TERMCAP_ue=$'\e[0m'     # Reset Underline

# Stolen from fish
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

alias c="clear"
alias just-shutdown="sudo shutdown now"
alias just-reboot="sudo reboot now"
alias clang-cl="clang --driver-mode=cl"
alias x86_64-w64-mingw32-clang="clang --target=x86_64-w64-mingw32"
alias biosfw="sudo systemctl reboot --firmware-setup"
alias wine="MESA_DEBUG=silent EGL_LOG_LEVEL=fatal wine"
export WINEPATH="/opt/llvm-mingw/x86_64-w64-mingw32/bin"

# SSH agent
eval "$(ssh-agent -s)" > /dev/null 2>&1
ssh-add ~/.ssh/id_ed25519 > /dev/null 2>&1

# Python environment activate function
activate-venv() {
  local venv="${1:-venv}"
  local act="$HOME/$venv/bin/activate"
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
  local f_name f_ext
  f_name="$(basename "$1" | awk -F. 'BEGIN{OFS="_"} {if ($(NF-1) == "tar") {ext = $(NF-1) "." $NF; NF-=2} else {ext = $NF; NF--}; print $0}')"
  f_ext="$(echo "$1" | awk -F. '{if ($(NF-1) == "tar") {print $(NF-1) "." $NF} else {print $NF}}')"

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

PATH="${(j.:.)USER_PATHS}:$PATH"

source setup_cc > /dev/null 2>&1

activate-venv > /dev/null 2>&1

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
    local orphans
    orphans=$(pacman -Qdtq)

    if [[ -n $orphans ]]; then
      sudo pacman -Rns $orphans
    else
      echo "No orphan package."
    fi
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
