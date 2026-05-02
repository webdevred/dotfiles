#!/bin/sh
pool_name="$(cat ~/.config/xmonad/pool.txt)"
[ -z "$pool_name" ] && exit 0

pool_alloc=$(zpool list -H -o alloc "$pool_name")
pool_size=$(zpool list -H -o size "$pool_name")

echo "$pool_alloc/$pool_size $pool_name"
