#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if ! [[ -x "$(command -v brew)" ]]; then
    /usr/bin/ruby -e "$(curl -fsSL \
            https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew analytics off

brew doctor

brew bundle --file="$SCRIPT_DIR/Brewfile"
