# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=tr_TR.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='nvim'
# fi

# Compilation flags
export ARCHFLAGS="-arch $(uname -m)"

# Set personal aliases, overriding those provided by Oh My Zsh libs,
# plugins, and themes. Aliases can be placed here, though Oh My Zsh
# users are encouraged to define aliases within a top-level file in
# the $ZSH_CUSTOM folder, with .zsh extension. Examples:
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

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
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

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
  read -s "pw?Şifre (boş bırak = GUI): "
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

# PATH entries
local -a paths=(
  "$HOME/Qt/6.10.2/gcc_64/bin"
  "$HOME/.local/share/ij-idea/bin"
  "$HOME/apache-maven-3.9.12/bin"
  "$HOME/.local/bin"
  "$HOME/.bun/bin"
)

for p in "${paths[@]}"; do
  [[ -d "$p" ]] && PATH="$p:$PATH"
done

# fnm
FNM_PATH="/home/ilpen/.local/share/fnm"
if [[ -d "$FNM_PATH" ]]; then
  export PATH="$FNM_PATH:$PATH"
  eval "$(fnm env)"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

activate-env

clear
