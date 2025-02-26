#!/bin/sh

for util in $(cat dest_places.txt); do        
    SRC=$(echo -n "$util" | awk -F ':' '{ print $1}')
    DST=$(echo -n "$util" | awk -F ':' '{ print $2}')
    if echo -n "$1" | grep -q 'u'; then
        rm -rf $(dirname "$HOME/$DST")
    else
        echo $SRC $HOME/$DST
        mkdir -p $(dirname "$HOME/$DST")
        ln -sf $(realpath $SRC) "$HOME/$DST"
    fi
done
