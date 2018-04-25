git_dirty() {
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    #command git diff --quiet --ignore-submodules HEAD &>/dev/null;
    command test -z "$(command git status --porcelain --ignore-submodules -unormal)"

    [[ $? -eq 1 ]] && echo " %F{red}(*)%f" && return

    echo " %F{green}(-)%f"
}

git_branch() {
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    echo " %F{245}[$(git rev-parse --abbrev-ref HEAD)]%f"
}

function zle-line-init zle-keymap-select {
    V_P="%F{green}[N]%f"
    RPROMPT="${${KEYMAP/vicmd/$V_P}/(main|viins)/}`git_branch``git_dirty`"
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

prompt_setup() {
    DEF_PROMPT="[%~]: "
    SSH_PROMPT="%F{245}(%F{003}%n%F{245}@%F{016}%m%F{245})%f ${DEF_PROMPT}"

    [[ -n $SSH_CONNECTION ]] && PROMPT="${SSH_PROMPT}" && return

    PROMPT="${DEF_PROMPT}"
}

prompt_setup
