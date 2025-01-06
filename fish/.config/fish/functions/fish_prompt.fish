function fish_prompt
    if not set -q VIRTUAL_ENV_DISABLE_PROMPT
        set -g VIRTUAL_ENV_DISABLE_PROMPT true
    end
    set_color magenta
    printf '%s@%s:%s' $USER (prompt_hostname) (prompt_pwd)
    set_color normal

    if test -n "$VIRTUAL_ENV"
        printf " (%s) " (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
    end

    set_color magenta
    echo -n '$ ' 
    set_color normal
end
