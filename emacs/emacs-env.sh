#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

rm "$SCRIPT_DIR/envfile"

env >> envfile

