# Prompt.zsh
#
# Note: Config variables are sourced from config.zsh

# https://github.com/sindresorhus/pretty-time-zsh
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

    # Output: Execution time and timestamp (ISO 8601)
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

check_git_branch_dirty() {
    builtin cd -q $1

    command git rev-parse --is-inside-work-tree &>/dev/null || return

    test -z "$(git status --porcelain -unormal)"
    [[ $? -ne 0 ]] && local dirty_marker="*"

    echo " %F{245}$(git rev-parse --abbrev-ref HEAD)${dirty_marker}%f"
}

check_jobs() {
    [[ -n $(jobs) ]] && echo " %F{yellow}+%f"
}

set_vi_status() {
    local V_P="%F{002}N%f"
    prompt_vi_status="${${KEYMAP/vicmd/$V_P}/(main|viins)/}"
    rprompt_draw
}

rprompt_draw() {
    RPROMPT="${prompt_vi_status}${prompt_git_status}`check_jobs`"

    zle reset-prompt
}

prompt_async_start() {
    # Flush previous jobs before starting a new one
    async_flush_jobs prompt_async_git_checker

    # Clear prompt
    prompt_git_status=""
    prompt_vi_status=""
    rprompt_draw

    # Start new job
    async_job prompt_async_git_checker check_git_branch_dirty $PWD
}

prompt_async_callback() {
    # Get result from the job exection
    prompt_git_status=$3

    # Redraw prompt
    rprompt_draw
}

prompt_async_init() {
    async_init
    async_start_worker prompt_async_git_checker -n
    async_register_callback prompt_async_git_checker prompt_async_callback
}

rprompt_simple() {
    local V_P="%F{002}N%f"
    RPROMPT="${${KEYMAP/vicmd/$V_P}/(main|viins)/}`check_jobs`"
    zle reset-prompt
}

prompt_setup() {
    zmodload zsh/datetime

    if (( $SET_ASYNC )); then
        zle -N zle-line-init prompt_async_start
        zle -N zle-keymap-select set_vi_status

        typeset -g prompt_vi_status # Stores latest keymap status
        typeset -g prompt_git_status # Stores latest git status

        prompt_async_init
    else
        zle -N zle-line-init rprompt_simple
        zle -N zle-keymap-select rprompt_simple
    fi

    PROMPT_EOL_MARK='' # Cleaner output if it doesn't end with a newline
    RPROMPT="" # Set this here so that RPROMPT works on the first line

    DEF_PROMPT="%B%(?..%F{001}!%f )%F{006}%~%f%b "
    USER_PROMPT="%n%F{245} at %F{004}%m%F{245} in%f ${DEF_PROMPT}"

    [[ -n $SSH_CONNECTION ]] && PROMPT="%F{004}${USER_PROMPT}" && return

    [[ $UID -eq 0 ]] && PROMPT="%F{001}${USER_PROMPT}" && return

    PROMPT="${DEF_PROMPT}"
}

prompt_setup
