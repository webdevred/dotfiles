#!/bin/sh
SELF_DIR="$(cd "$(dirname "$0")" && pwd)"

# Bygg om PATH utan wrapperns egen katalog
NEW_PATH=$(echo "$PATH" | tr ':' '\n' | grep -vFx "$SELF_DIR" | tr '\n' ':')
REAL_CABAL=$(PATH="$NEW_PATH" command -v cabal)

if [ -z "$REAL_CABAL" ]; then
  echo "cabal-wrapper: kunde inte hitta riktiga cabal" >&2
  exit 1
fi

args=()
for a in "$@"; do
  [ "$a" = "--keep-temp-files" ] && continue
  args+=("$a")
done

exec "$REAL_CABAL" "${args[@]}"
