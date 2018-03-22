# Use colors for 'ls'
if ls --color > /dev/null 2>&1; then
    alias ls='ls --color'
else
    alias ls='ls -G'
fi

# => Aliases
alias cl='clear'

alias g='git'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# => MacOS
alias showAll='defaults write com.apple.finder AppleShowAllFiles YES && killall Finder'
alias hideAll='defaults write com.apple.finder AppleShowAllFiles NO && killall Finder'

