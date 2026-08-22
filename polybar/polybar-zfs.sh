#!/bin/sh
pool_names="$(cat ~/.config/xmonad/pool.txt)"
[ -z "$pool_names" ] && exit 0

for pool_name in $pool_names; do
  pool_alloc=$(zpool list -H -o alloc "$pool_name")
  pool_size=$(zpool list -H -o size "$pool_name")
  printf '%s/%s %s ' "$pool_alloc" "$pool_size" "$pool_name"
done
echo
