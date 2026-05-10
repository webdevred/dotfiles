[[ -r /usr/share/bash-completion/bash_completion ]] &&
  . /usr/share/bash-completion/bash_completion

git-remove-orphaned-branches() {
  git -C "$PWD" rev-parse --is-inside-work-tree &>/dev/null || {
    echo "Not a git repo: $PWD" >&2; return 1
  }

  local dry=0
  [[ "$1" == "-n" || "$1" == "--dry-run" ]] && dry=1

  local default
  default=$(git -C "$PWD" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null \
            | sed 's|^origin/||')
  [[ -z "$default" ]] && { echo "Could not determine default branch." >&2; return 1; }

  git -C "$PWD" fetch --prune --quiet

  local orphans=() branch
  while IFS= read -r branch; do
    [[ -z "$branch" || "$branch" == "$default" ]] && continue
    # has commits not reachable from origin/default?
    if [[ -n $(git -C "$PWD" log "origin/${default}..${branch}" --oneline 2>/dev/null) ]]; then
      orphans+=("$branch")
    fi
  done < <(git -C "$PWD" for-each-ref \
             --format='%(refname:short) %(upstream:track)' refs/heads \
           | awk '/\[gone\]/ {print $1}')

  if (( ${#orphans[@]} == 0 )); then
    echo "No orphaned local branches with unpushed changes."
    return 0
  fi

  printf 'Orphaned branches with unpushed changes:\n'
  printf '  %s\n' "${orphans[@]}"
  (( dry )) && return 0

  printf '\nDelete these? [y/N] '
  read -r confirm
  if [[ "$confirm" =~ ^[Yy]$ ]]; then
    git -C "$PWD" branch -D "${orphans[@]}"
  else
    echo "Aborted."
  fi
}

kill_my_procs() {
  if [[ -z $1 ]]; then
    echo "please supply software to kill processes for"
    return 1
  fi

  ps faux | grep $1 | awk '{ print $2 }' | xargs kill -9 2>/dev/null
}

alias ncdu='ncdu -x'
alias df='df -x efivarfs -x tmpfs -x zfs -x devtmpfs -hT'
alias lsblk='lsblk -e 7'

if declare -f __git_ps1 >/dev/null 2>&1 &&
  ! declare -f _orig___git_ps1 >/dev/null 2>&1; then
  eval "_orig_$(declare -f __git_ps1)"
  __git_ps1() { _orig___git_ps1 "$@" | tr -d '\0'; }
fi

HISTSIZE=100000
HISTFILESIZE=200000
HISTCONTROL=ignoredups:erasedups
HISTTIMEFORMAT="%F %T  "
shopt -s histappend checkwinsize globstar
PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"

export LESS="-N -M -R --shift 5"
export EDITOR="vi"
export MANPAGER='less -R --use-color -Dd+r -Du+b'
export PATH="~/.local/bin:$PATH"

declare -f __git_ps1 >/dev/null 2>&1 || __git_ps1() { :; }

PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[38;2;255;170;255m\]\w\[\033[36m\]$(__git_ps1)\[\033[0m\] $ '
