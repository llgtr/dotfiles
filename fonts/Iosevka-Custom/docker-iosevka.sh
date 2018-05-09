#!/usr/bin/env bash

IOSEVKA_BUILD_ARGS=("term"
                    "cv01"
                    "cv04"
                    "cv08"
                    "cv11"
                    "cv14"
                    "cv17"
                    "cv19"
                    "cv20"
                    "cv22"
                    "cv26"
                    "cv29"
                    "cv33"
                    "cv35"
                    "cv37"
                    "cv38"
                    "cv40"
                    "cv42"
                    "cv44"
                    "cv47"
                    "cv49"
                    "cv50"
                    "cv52")

git pull

npm i

make custom-config design="${IOSEVKA_BUILD_ARGS[*]}"
make custom
