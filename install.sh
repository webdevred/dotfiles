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
    ghc --run $dot_file_dir/configure.hs -- $2
  fi
}

if echo -n "$1" | grep -Eq '^-?i$' && [ -n $2 ]; then
  action=1
  util_to_install="$2"
elif echo -n "$1" | grep -Eq '^-?u$'; then
  action=2
  util_to_remove="$2"
elif echo -n "$1" | grep -Eq '^-?c$'; then
  action=3
  util_to_configure="$2"
else
  action=4
fi

for util in $(cat dest_places.txt | sed -E '/^(#.*)?$/d'); do
  dot_file=$(echo -n "$util" | awk -F ':' '{ print $1}')
  config_dst=$(echo -n "$util" | awk -F ':' '{ print $2}')
  dot_file_dir=$(echo -n "$dot_file" | sed 's/\/.*//')
  config_dst_base_dir=$(echo -n "$config_dst" | grep -oh '^.config/[^/]*')

  if [ -z "$dot_file" ] || [ -z "$config_dst" ]; then
    echo "invalid line $util"
    continue
  fi

  if [ $action -eq 4 ] || { [ $action -eq 1 ] && echo -n "$dot_file" | grep -Eq '^'"$util_to_install"''; }; then
    if [ -d "$dot_file" ]; then
      for file in $(find "$dot_file" -type f -not -name '.*' -not -name '*~'); do
        single_filename=$(realpath --relative-to="$dot_file" "$file")
        single_dst_path="$HOME/$config_dst/$single_filename"
        dottfiles_link "$(realpath $file)" "$single_dst_path"
      done
    else
      real_dotfile_path=$(realpath "$dot_file")
      dottfiles_link "$real_dotfile_path" "$HOME/$config_dst"
    fi
  elif [ $action -eq 2 ] && { [ -z "$util_to_remove" ] || echo -n "$dot_file" | grep -Eq '^'"$util_to_remove"''; }; then
    if [ -n "$config_dst_base_dir" ] && [ -d "$HOME/$config_dst_base_dir" ]; then
      dotfiles_remove "$HOME/$config_dst_base_dir"
    else
      dotfiles_remove "$HOME/$config_dst"
    fi
  fi

  if [ $action -eq 3 ] && echo -n "$dot_file" | grep -Eq '^'"$util_to_configure"'' && [ -d "$dot_file_dir" ]; then
    dotfiles_configure "$dot_file_dir" "$HOME/$config_dst_base_dir"
  fi
done
