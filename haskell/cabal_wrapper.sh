#!/bin/bash

set -euo pipefail

wrapper_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

IFS=':' read -r -a _parts <<<"$PATH"
new_path=""
for p in "${_parts[@]}"; do
  if [[ "$p" != "$wrapper_dir" ]]; then
    if [[ -z "$new_path" ]]; then new_path="$p"; else new_path="$new_path:$p"; fi
  fi
done

given_command="${1:-}"
is_interactive=false
if [[ -n "$given_command" && "repl" == "$given_command" ]]; then
  is_interactive=true
fi

real_cabal="$(PATH="$HOME/.ghcup/bin:$new_path" command -v cabal || true)"
if [[ -z "$real_cabal" ]]; then
  echo "wrapper: couldnt find the real 'cabal' in PATH." >&2
  exit 2
fi

export PATH="$PATH:$HOME/.ghcup/bin"
if [[ -n "${CABAL_WRAPPER:-}" ]] || [[ -n "${MY_HLS_WRAPPER:-}" ]]; then
  exec "$real_cabal" "$@"
fi

max_jobs=$(($(nproc) - 2))
((max_jobs < 1)) && max_jobs=1

build_commands=" build install rebuild repl v2-repl test "
given_command="${1:-}"

if [[ "$build_commands" == *" $given_command "* ]]; then
  printf "wrapper: building with --jobs=%s\n" "$max_jobs"
  extra_args+=("--jobs=$max_jobs" "--upgrade-dependencies")

  if [[ -f "cabal.project.dev" ]]; then
    extra_args+=("--project-file=cabal.project.dev")
  fi
  if [[ "$given_command" == "install" ]]; then
    extra_args+=("--overwrite-policy=always" "--installdir=$HOME/.local/bin")
  fi
  printf "wrapper: passing extra arguments to cabal:\n "
  printf "%s " "${@:2}" "${extra_args[@]}"
  printf "\n\n"
fi

if $is_interactive; then
  exec "$real_cabal" "$given_command" "${@:2}" "${extra_args[@]}"
  rc=$?
else
  "$real_cabal" "$given_command" "${@:2}" "${extra_args[@]}" &
  child=$!
  trap 'kill -TERM "$child" 2>/dev/null || true' INT TERM
  wait "$child"
  rc=$?
fi

printf "wrapper: cabal exited with status %d\n" "$rc"
exit $rc
