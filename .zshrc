if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"
export ZSH_COMPDUMP="${XDG_CACHE_HOME:-$HOME/.cache}/zcompdump"

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
ENABLE_CORRECTION="true"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# PATH entries
local -a USER_PATHS=(
  "$HOME/Qt/6.10.2/gcc_64/bin"
  "$HOME/.local/share/ij-idea/bin"
  "$HOME/apache-maven-3.9.12/bin"
  "$HOME/.local/bin"
  "$HOME/.bun/bin"
  "$HOME/.local/go/bin"
)

export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=tr_TR.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
export ARCHFLAGS="-arch $(uname -m)"

alias la="ls -ah"
alias ll="ls -lh"
alias lla="ls -lah"
alias c="clear"
alias just-shutdown="sudo shutdown now"
alias just-reboot="sudo reboot now"
alias clang-cl="clang --driver-mode=cl"
alias ..="cd .."
alias .="cd ."

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

# quick built-in password feeder into keepassxc
kp() {
  local db="${1:-$HOME/ŞİFRELER.kdbx}"
  [[ -f "$db" ]] || { echo "No such file: $db"; return 1; }
  read -s "pw?Password (empty = GUI): "
  echo
  if [[ -z "$pw" ]]; then
    keepassxc "$db" &
  else
    printf '%s' "$pw" | keepassxc --pw-stdin "$db" &
  fi
}

# "cmake" command for windows x86_64 environment
cmakewin() {
  cmake \
    -DCMAKE_SYSTEM_NAME=Windows \
    -DCMAKE_C_COMPILER=x86_64-w64-mingw32-gcc \
    -DCMAKE_CXX_COMPILER=x86_64-w64-mingw32-g++ \
    "$@"
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

alias docker-set-permits="sudo chown -R ilpen:ilpen /home/ilpen/docker_data"
alias psqlconn="psql \"postgresql://postgres:postgres@127.0.0.1:54322/postgres\""
alias psqlrunq="PGPASSWORD=postgres psql -h 127.0.0.1 -p 54322 -U postgres -d postgres -f"

PATH="${(j.:.)USER_PATHS}:$PATH"

source setup_cc > /dev/null 2>&1

# fnm
FNM_PATH="/home/ilpen/.local/share/fnm"
if [[ -d "$FNM_PATH" ]]; then
  export PATH="$FNM_PATH:$PATH"
  eval "$(fnm env)"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

activate-venv > /dev/null 2>&1
