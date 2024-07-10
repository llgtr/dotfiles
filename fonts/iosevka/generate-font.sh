#!/usr/bin/env bash

npm install

# This also generates the unhinted TTF files
npm run build -- woff2-unhinted::IosevkaFacile
