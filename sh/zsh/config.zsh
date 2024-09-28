export EDITOR=vim

# Vi mode
bindkey -v

# Init better autocomplete
autoload -Uz compinit
_zcomp_path="${HOME}/.zcompdump"
# Match if modified within 20 hours
if [[ $_zcomp_path(#qNmh-20) ]]; then
    compinit -C
else
    compinit -i
    touch "$_zcomp_path"
fi
unset _zcomp_path

zstyle ':completion:*' menu select
zstyle ':completion:*'  list-colors '=*=90' # Color all completions bright gray
zstyle ':completion::complete:*' use-cache on # Make slower completions usable
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'

REPORTTIME=10 # Will show a readout when system+user time passes the value
REPORTTIME_TOTAL=10 # Will show a readout when total time passes the value
SET_ASYNC=1 # Set this to zero to disable async prompt

KEYTIMEOUT=1

# => History
HISTFILE=~/.zhistory

HISTSIZE=8000
SAVEHIST=8000

bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

setopt APPEND_HISTORY # Append rather than replace hist file
setopt INC_APPEND_HISTORY # Incrementally append commands when entered
setopt HIST_IGNORE_ALL_DUPS # Only keep most recent duplicate of command
setopt HIST_REDUCE_BLANKS # Remove superfluous blanks
setopt EXTENDED_HISTORY # Save commands timestamp and duration
setopt HIST_VERIFY # Dont execute outright when entering line w/ hist expansion

# => Basics
setopt CHASE_LINKS # Resolve symlinks to their true values
setopt NO_LIST_BEEP # Dont beep on an ambiguous completion
setopt PROMPT_SUBST # Allow parameter/arithmetic expansion & command substitution
setopt AUTO_MENU # Show completion menu after a successive tab press
setopt COMPLETE_IN_WORD # Complete from both ends of a word.
setopt ALWAYS_TO_END # Move cursor to the end of a completed word.
setopt PATH_DIRS # Perform path search even on command names with slashes.
setopt AUTO_LIST # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH # If completed parameter is a directory, add a trailing slash.
setopt EXTENDED_GLOB # Needed for file modification glob modifiers with compinit.
unsetopt MENU_COMPLETE # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL # Disable start/stop characters in shell editor.

