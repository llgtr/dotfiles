git_branch() {
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    echo " %F{245}$(git rev-parse --abbrev-ref HEAD)%f"
}

rprompt_vi_git() {
    V_P="%F{002}N%f"
    RPROMPT="${${KEYMAP/vicmd/$V_P}/(main|viins)/}`git_branch`"
    zle reset-prompt
}

cmd_execution_time() {
    local stop=$EPOCHSECONDS
    (( elapsed = ${stop} - ${cmd_start_time:-$EPOCHSECONDS} ))

	local human total_seconds=$elapsed
	local days=$(( total_seconds / 60 / 60 / 24 ))
	local hours=$(( total_seconds / 60 / 60 % 24 ))
	local minutes=$(( total_seconds / 60 % 60 ))
	local seconds=$(( total_seconds % 60 ))
	(( days > 0 )) && human+="${days}d "
	(( hours > 0 )) && human+="${hours}h "
	(( minutes > 0 )) && human+="${minutes}m "
    human+="${seconds}s"

    (( $elapsed > $REPORTTIME_TOTAL )) && print -P \
        "%F{yellow}${human}%f %F{245}-> $(date -u +"%Y-%m-%dT%H:%M:%SZ")%f"
}

preexec() {
    cmd_start_time=$EPOCHSECONDS
}

precmd() {
    if (($+cmd_start_time)); then
        cmd_execution_time
    fi
}

prompt_setup() {
    zmodload zsh/datetime

    zle -N zle-line-init rprompt_vi_git
    zle -N zle-keymap-select rprompt_vi_git

    PROMPT_EOL_MARK=''

    DEF_PROMPT="%B%F{006}%~%f%b "
    SSH_PROMPT="%F{245}(%F{003}%n%F{245}@%F{016}%m%F{245})%f ${DEF_PROMPT}"

    [[ -n $SSH_CONNECTION ]] && PROMPT="${SSH_PROMPT}" && return

    PROMPT="${DEF_PROMPT}"
    RPROMPT=""
}

prompt_setup
