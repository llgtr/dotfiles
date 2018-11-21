#!/usr/bin/env bash

set -euo pipefail

EXTENSIONS=(
    ms-vscode.Theme-TomorrowKit
    vscodevim.vim
)

for i in "${EXTENSIONS[@]}"; do
    code --install-extension "$i" || continue
done
