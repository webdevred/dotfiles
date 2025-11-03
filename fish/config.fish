set -g fish_greeting

set -x EDITOR vim
set -x SYSTEMD_EDITOR vim
set -x LESS '-N -M -R --shift 5'

set -x WPC_DEV dev

set -U fish_autosuggestion_enabled 0
set -x vi

fish_add_path ~/.local/bin

if status is-interactive
    # Commands to run in interactive sessions can go here
    stack --fish-completion-script (which stack) >/dev/null
end

function ncdu
    command ncdu -x $argv
end

function df
    command df -x efivarfs -x tmpfs -x zfs -hT $argv
end

function lsblk
    command lsblk -e 7 $argv
end

function fish_title
    printf "terminal: %s" (pwd)
end

function cabal_gild_all
    set files (find . -name 'cabal.project.*' -o -name 'cabal.project')
    for file in $files
        printf "formatting %s\n" $file
        command cabal-gild --io $file
    end
end

function fish_prompt
    if not set -q VIRTUAL_ENV_DISABLE_PROMPT
        set -g VIRTUAL_ENV_DISABLE_PROMPT true
    end
    set_color "#ffaaff"
    printf '%s@%s:%s' $USER (prompt_hostname) (prompt_pwd)
    set_color normal

    if test -n "$VIRTUAL_ENV"
        printf " (%s) " (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
    end

    set_color magenta
    echo -n '$ '
    set_color normal
end

function myyamlfix
    set -l yaml_files (find . \( -name "*.yaml" -o -name ".*.yaml" -o -name "*.yml" -o -name ".*.yml" \) -type f -not -name ".hlint.yaml" -not -path '*/node_modules/*')
    if test (count $yaml_files) -eq 0
        echo "No YAML files found."
        return 1
    end
    yamlfix -c $HOME/.config/yamlfix/config.toml $yaml_files $argv
end
