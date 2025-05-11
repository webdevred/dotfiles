install_file_and_log() {
    install_file | while IFS= read -r line; do
        echo "$file > $line"
    done
}
install_file() {
  if echo -n "$file" | grep -q 'haskell-mode-git.el'; then
    external_package_dir="$HOME/$config_dst_base_dir/opt"
    mkdir -p $external_package_dir
    if [ -d "$external_package_dir/haskell-mode" ]; then
      cd "$external_package_dir/haskell-mode"
      git pull
      make
      { cd -; } >/dev/null
    else
      cd $external_package_dir
      git clone "https://github.com/haskell/haskell-mode.git"
      make
      { cd -; } >/dev/null
    fi
  fi

  mkdir -p "$HOME/$config_dst_base_dir/extra-enabled"
  ln -sf "$real_dot_file_filename/extra/$file" "$HOME/$config_dst_base_dir/extra-enabled/$file"
  sleep 1
  echo "done with $file"
}

for file in $(ls emacs/extra | grep -v '~'); do
  read -p "Do you want $file? " yn
  case $yn in
      [Yy]*) install_file_and_log ;;
  *) ;;
  esac
done
