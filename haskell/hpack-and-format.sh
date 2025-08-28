#!/bin/bash

if ! hpack -f --canonical >/dev/null; then
  echo "Problem with hpack"
  exit 1
fi

cabal_file=$(hpack -f --canonical | awk '{ print $2 }')

comment_block=$(awk '/^-- This file has been generated from package.yaml by hpack version/ { inBlock=1 }
                    inBlock && /^--/ { print; next }
                    inBlock && !/^--/ { exit }' "$cabal_file")

sed -i '/^-- This file has been generated from package.yaml by hpack version/,/^--[^-]/d' "$cabal_file"

cabal format "$cabal_file"

awk -v block="$comment_block" '
  /^cabal-version:/ {
    print
    printf "\n"
    print block
    printf "\n"
    next
  }
  {
    print
  }
' "$cabal_file" > tmpfile && mv tmpfile "$cabal_file"

echo "Generated and formatted $cabal_file"
