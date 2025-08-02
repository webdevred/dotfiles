set -g fish_greeting

set -x EDITOR vim
set -x SYSTEMD_EDITOR vim
set -x LESS '-N -M -R --shift 5'

set -U fish_autosuggestion_enabled 0
set -x vi

set -U fish_user_paths ~/.local/bin $fish_user_paths

if status is-interactive
    # Commands to run in interactive sessions can go here
    stack --fish-completion-script (which stack) >/dev/null
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

function cabal
    set -l max_jobs (math (nproc) - 2)
    if test $max_jobs -lt 1
        set max_jobs 1
    end

    set -l build_commands build install rebuild repl

    if test (count $argv) -ge 1
        if contains -- $argv[1] $build_commands
            printf "fish: building with --jobs=%s\n" $max_jobs
            if test $argv[1] = install
                set -l extra_flags \
                    --overwrite-policy=always \
                    --upgrade-dependencies \
                    --installdir=$HOME/.local/bin
                command cabal install --jobs=$max_jobs $extra_flags $argv[2..-1]
            else
                command cabal $argv[1] --jobs=$max_jobs $argv[2..-1]
            end
        else
            command cabal $argv
        end
    else
        command cabal
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
    set -l yaml_files (find . \( -name "*.yaml" -o -name ".*.yaml" -o -name "*.yml" -o -name ".*.yml" \) -type f -not -name ".hlint.yaml")
    if test (count $yaml_files) -eq 0
        echo "No YAML files found."
        return 1
    end
    yamlfix -c $HOME/.config/yamlfix/config.toml $yaml_files $argv
end
