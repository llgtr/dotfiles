#!/usr/bin/env bash

EXTENSIONS=(
    ms-vscode.Theme-TomorrowKit
    vscodevim.vim
)

for i in "${EXTENSIONS[@]}"; do
    code --install-extension "$i" || continue
done
