#!/usr/bin/env bash

git pull

npm i

npm run build -- contents::iosevka-custom
