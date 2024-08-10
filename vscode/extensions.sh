#!/usr/bin/env bash

set -euo pipefail

EXTENSIONS=(
    jdinhlife.gruvbox
    vscodevim.vim
    VSpaceCode.whichkey
    esbenp.prettier-vscode
)

for i in "${EXTENSIONS[@]}"; do
    code --install-extension "$i" || continue
done
