#!/bin/bash

set -euo pipefail

wrapper_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

IFS=':' read -r -a _parts <<< "$PATH"
new_path=""
for p in "${_parts[@]}"; do
  if [[ "$p" != "$wrapper_dir" ]]; then
    if [[ -z "$new_path" ]]; then new_path="$p"; else new_path="$new_path:$p"; fi
  fi
done

real_cabal="$(PATH="$new_path" command -v cabal || true)"
if [[ -z "$real_cabal" ]]; then
  echo "wrapper: couldnt find the real 'cabal' in PATH." >&2
  exit 2
fi

if [[ -n "${CABAL_WRAPPER:-}" ]]; then
  exec "$real_cabal" "$@"
fi

max_jobs=$(( $(nproc) - 2 ))
(( max_jobs < 1 )) && max_jobs=1

build_commands="build install rebuild repl"
given_command="${1:-}"

if [[ -n "$given_command" && " $build_commands " == *" $given_command "* ]]; then
    printf "wrapper: building with --jobs=%s\n" "$max_jobs"

    extra_args=( "--jobs=$max_jobs" "--upgrade-dependencies" )
    [[ -f "cabal.project.dev" ]] && extra_args+=( "--project-file=cabal.project.dev" )
    if [[ "$given_command" == "install" ]]; then
        extra_args+=( "--overwrite-policy=always" "--installdir=$HOME/.local/bin" )
    fi

    printf "wrapper: passing extra arguments to cabal:\n  %s\n\n" "${extra_args[*]}"

    "$real_cabal" "$given_command" "${@:2}" "${extra_args[@]}" &
    child=$!

else
    "$real_cabal" "$@" &
    child=$!
fi

trap 'kill -TERM "$child" 2>/dev/null || true' INT TERM
wait "$child"
rc=$?

printf "wrapper: cabal exited with status %d\n" "$rc"
exit $rc
