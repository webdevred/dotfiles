_git_default_branch() {
  git -C "$PWD" symbolic-ref refs/remotes/origin/HEAD 2>/dev/null |
    sed 's|refs/remotes/origin/||' ||
    git -C "$PWD" remote show origin 2>/dev/null |
    awk '/HEAD branch/ {print $NF}'
}

git-list-orphaned-branches() {
  git -C "$PWD" rev-parse --is-inside-work-tree &>/dev/null || {
    echo "Not a git repo: $PWD"
    return 1
  }

  local default_branch
  default_branch=$(_git_default_branch)
  [[ -z "$default_branch" ]] && {
    echo "Could not determine default branch." >&2
    return 1
  }

  git -C "$PWD" fetch --prune

  local remote_branches
  remote_branches=$(git -C "$PWD" branch -r | awk '{print $1}')

  local branch
  git -C "$PWD" branch -vv | grep origin |
    awk '{print $1}' |
    while IFS= read -r branch; do
      if ! echo "$remote_branches" | grep -qF "$branch"; then
        if [[ -n $(git -C "$PWD" diff "origin/${default_branch}...${branch}" -- .) ]]; then
          echo "$branch"
        fi
      fi
    done
}

git-remove-orphaned-branches() {
  git -C "$PWD" rev-parse --is-inside-work-tree &>/dev/null || {
    echo "Not a git repo: $PWD"
    return 1
  }

  local branches
  branches=$(git-list-orphaned-branches)

  if [[ -z "$branches" ]]; then
    echo "No orphaned local branches with changes under $PWD."
    return 0
  fi

  echo "Orphaned branches with unpushed changes under $PWD to delete:"
  echo "${branches//^ /}"
  printf '\nConfirm deletion? [y/N] '
  read -r confirm
  if [[ "$confirm" =~ ^[Yy]$ ]]; then
    echo "$branches" | xargs git -C "$PWD" branch -D
  else
    echo "Aborted."
  fi
}

alias ncdu='ncdu -x'
alias df='df -x efivarfs -x tmpfs -x zfs -x devtmpfs -hT'
alias lsblk='lsblk -e 7'

if declare -f __git_ps1 >/dev/null 2>&1 \
   && ! declare -f _orig___git_ps1 >/dev/null 2>&1; then
  eval "_orig_$(declare -f __git_ps1)"
  __git_ps1() { _orig___git_ps1 "$@" | tr -d '\0'; }
fi

declare -f __git_ps1 >/dev/null 2>&1 || __git_ps1() { :; }

PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[38;2;255;170;255m\]\w\[\033[36m\]$(__git_ps1)\[\033[0m\] $ '
