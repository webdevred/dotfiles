#!/bin/sh

dottfiles_link() {
  config_dst_dir="$(dirname "$2")"

  if ! [ -d "$config_dst_dir" ]; then
    set -x
    mkdir -p "$config_dst_dir"
    { set +x; } 2>/dev/null
  fi

  if ! [ -e "$2" ]; then
    set -x
    ln -sf "$1" "$2"
    { set +x; } 2>/dev/null
  fi
}

dotfiles_remove() {
  if ! [ -e $1 ]; then
    return
  fi
  set -x
  rm -rf "$1"
  { set +x; } 2>/dev/null
}

dotfiles_configure() {
  if [ -f "$dot_file_dir/configure.sh" ]; then
    . $dot_file_dir/configure.sh
  elif [ -f "$dot_file_dir/configure.hs" ]; then
    ghc -fforce-recomp --run $dot_file_dir/configure.hs -- $2
  fi
}

if echo -n "$1" | grep -Eq '^-?i$' && [ -n $2 ]; then
  action=1
  selected_util="$2"
elif echo -n "$1" | grep -Eq '^-?u$'; then
  action=2
  selected_util="$2"
elif echo -n "$1" | grep -Eq '^-?c$'; then
  action=3
  selected_util="$2"
else
  action=4
fi

for util in $(cat dest_places.txt | sed -E '/^(#.*)?$/d'); do
  dot_file=$(echo -n "$util" | awk -F ':' '{ print $1}')
  config_dst=$(echo -n "$util" | awk -F ':' '{ print $2}')
  mode=$(echo -n "$util" | awk -F ':' '{ print $4}')
  dot_file_dir=$(echo -n "$dot_file" | sed 's/\/.*//')
  config_dst_base_dir=$(echo -n "$config_dst" | grep -oh '^.config/[^/]*')

  real_dot_file_filename=$(realpath $dot_file)

  if [ -z "$dot_file" ] || [ -z "$config_dst" ]; then
    echo "invalid line $util"
    continue
  fi

  if [ $action -eq 4 ] || { [ $action -eq 1 ] && echo -n "$dot_file" | grep -Eq '^'"$util_to_install"''; }; then
    if [ -d "$dot_file" ]; then
      for file in $(find "$dot_file" -type f -not -name '.*' -not -name '*~' -not -name '#*'); do
        single_filename=$(realpath --relative-to="$dot_file" "$file")
        single_dst_path="$HOME/$config_dst/$single_filename"

        single_real_dotfile_path=$(realpath "$file")

        dottfiles_link "$single_real_dotfile_path" "$single_dst_path"
      done
    elif [ -f "$dot_file" ]; then
      dottfiles_link "$real_dotfile_path" "$HOME/$config_dst"

      current_mode=$(stat -c '%a' $dot_file)
      if echo -n "$mode" | grep -Eq '[0-7]{3}' && echo -n "$mode" | grep -vq "$current_mode"; then
        set -x
        chmod $mode $real_dotfile_path
        { set +x; } 2>/dev/null
      fi
    fi
  elif [ $action -eq 2 ] && { [ -z "$selected_util" ] || echo -n "$dot_file" | grep -Eq '^'"$selected_util"''; }; then
    if [ -n "$config_dst_base_dir" ] && [ -d "$HOME/$config_dst_base_dir" ]; then
      dotfiles_remove "$HOME/$config_dst_base_dir"
    else
      dotfiles_remove "$HOME/$config_dst"
    fi
  fi

  if [ $action -eq 3 ] && echo -n "$dot_file" | grep -Eq '^'"$selected_util"'' && [ -d "$dot_file_dir" ]; then
    dotfiles_configure "$dot_file" "$HOME/$config_dst_base_dir"
  fi
done
