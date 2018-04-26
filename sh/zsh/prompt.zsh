git_branch() {
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    echo " %F{245}$(git rev-parse --abbrev-ref HEAD)%f"
}

check_jobs() {
    [[ -n $(jobs) ]] && echo " %F{yellow}+%f"
}

rprompt_vi_git() {
    local V_P="%F{002}N%f"
    RPROMPT="${${KEYMAP/vicmd/$V_P}/(main|viins)/}`git_branch``check_jobs`"
    zle reset-prompt
}

# Taken from the pure prompt
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

    PROMPT_EOL_MARK='' # Cleaner output if it doesn't end with a newline
    RPROMPT="" # Set this here so that RPROMPT works on the first line

    DEF_PROMPT="%B%(?..%F{001}!%f )%F{006}%~%f%b "
    USER_PROMPT="%n%F{245} at %F{004}%m%F{245} in%f ${DEF_PROMPT}"

    [[ -n $SSH_CONNECTION ]] && PROMPT="%F{004}${USER_PROMPT}" && return

    [[ $UID -eq 0 ]] && PROMPT="%F{001}${USER_PROMPT}" && return

    PROMPT="${DEF_PROMPT}"
}

prompt_setup
