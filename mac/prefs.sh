#!/usr/bin/env bash

set -eo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

for i in $SCRIPT_DIR/plists/*; do
    filename="$(basename $i)"
    dst_file="$HOME/Library/Preferences/$filename"
    case "$1" in
        "export")
            plutil -convert xml1 "$dst_file" -o "$i"
            ;;
        "import")
            plutil -convert binary1 "$i" -o "$dst_file"
            killall cfprefsd
            ;;
        *)
            echo "Usage: ./prefs.sh <export|import>"
            exit
            ;;
    esac
done
