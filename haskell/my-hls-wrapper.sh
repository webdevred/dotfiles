#!/bin/bash

set -euo pipefail

if ! command -v ghc >/dev/null 2>&1; then
  echo "Error: GHC is not installed or not in PATH." >&2
  exit 1
fi

ghc_version=$(ghc --numeric-version)
echo "GHC version: $ghc_version" >&2

if [ -d "$HOME/.local/state/cabal/store" ]; then
  base_path="$HOME/.local/state/cabal"
elif [ -d "$HOME/.cabal/store" ]; then
  base_path="$HOME/.cabal"
else
  echo "Warning: Could not find Cabal store directory." >&2
fi

hspec_path=$(find "$base_path/store/ghc-$ghc_version" -type f -name hspec-discover -exec dirname {} \; | head -n 1 || true)

if [ -n "$hspec_path" ] && [ -x "$hspec_path/hspec-discover" ]; then
  echo "Found hspec-discover at: $hspec_path" >&2
  export PATH="$hspec_path:$PATH"
else
  echo "Warning: hspec-discover not found for GHC $ghc_version. Continuing without it." >&2
fi

echo "PATH is now: $PATH" >&2

if command -v haskell-language-server >/dev/null 2>&1; then
  exec haskell-language-server --lsp
else
  echo "Error: haskell-language-server not found in PATH." >&2
  exit 1
fi
