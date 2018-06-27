# Color scheme helper
BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# => General configs
export EDITOR=vim

# Vi mode
bindkey -v

# Init better autocomplete
autoload -U compinit
compinit
zstyle ':completion:*' insert-tab pending

REPORTTIME=10 # Will show a readout when system+user time passes the value
REPORTTIME_TOTAL=10 # Will show a readout when total time passes the value
SET_ASYNC=1 # Set this to zero to disable async prompt

KEYTIMEOUT=1

# => History
HISTFILE=~/.zhistory

HISTSIZE=2000
SAVEHIST=2000

setopt APPEND_HISTORY # Append rather than replace hist file
setopt INC_APPEND_HISTORY # Incrementally append commands when entered
setopt HIST_IGNORE_ALL_DUPS # Only keep most recent duplicate of command
setopt HIST_REDUCE_BLANKS # Remove superfluous blanks
setopt EXTENDED_HISTORY # Save commands timestamp and duration
setopt HIST_VERIFY # Dont execute outright when entering line w/ hist expansion
setopt CHASE_LINKS # Resolve symlinks to their true values

# => Basics
setopt NO_LIST_BEEP # Dont beep on an ambiguous completion
setopt PROMPT_SUBST # Allow parameter/arithmetic expansion & command substitution
setopt AUTO_MENU
