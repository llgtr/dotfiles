#!/usr/bin/env bash

ENVFILE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/envfile"

if [[ -e "$ENVFILE" ]]; then
    printf "The envfile already exists. Do you want to override [y/n]? "
    read -r -n 1
    if [[ ! "$REPLY" =~ [yY] ]]; then
        exit
    fi
    rm "$ENVFILE"
fi

env >> envfile

