#!/bin/bash
SELF_DIR="$(cd "$(dirname "$0")" && pwd)"
NEW_PATH=$(echo "$PATH" | sed "s|~|$HOME|g" | tr ':' '\n' | grep -vFx "$SELF_DIR" | tr '\n' ':')
hash -r
REAL_CABAL=$(PATH="$NEW_PATH" command -v cabal)

CABAL_PROJECT_DEV="cabal.project.dev"

export TERM=xterm-256color
if [ -z "$REAL_CABAL" ]; then
  echo "cabal-wrapper: kunde inte hitta riktiga cabal" >&2
  exit 1
fi

args=()
has_project_file=0
for a in "$@"; do
  [ "$a" = "--keep-temp-files" ] && continue
  case "$a" in
    --project-file | --project-file=* | --project-dir | --project-dir=*)
      has_project_file=1
      ;;
  esac
  args+=("$a")
done

if [ "$has_project_file" -eq 0 ] && [[ -f "$CABAL_PROJECT_DEV" ]]; then
  args+=("--project-file=${CABAL_PROJECT_DEV}")
fi

exec "$REAL_CABAL" "${args[@]}"
