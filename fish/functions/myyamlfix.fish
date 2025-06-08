function myyamlfix
  set -l yaml_files (find . \( -name "*.yaml" -o -name ".*.yaml" \) -type f -not -name ".hlint.yaml")
  if test (count $yaml_files) -eq 0
    echo "No YAML files found."
    return 1
  end
  yamlfix -c ~/.config/yamlfix/config $yaml_files $argv
end
