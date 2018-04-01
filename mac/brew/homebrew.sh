#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

set -e

if ! [[ -x "$(command -v brew)" ]]; then
    /usr/bin/ruby -e "$(curl -fsSL \
            https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew doctor

brew bundle --file="$SCRIPT_DIR/Brewfile"
