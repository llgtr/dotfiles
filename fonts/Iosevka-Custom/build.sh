#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

rm -rf $SCRIPT_DIR/out/
docker build -t iosevka-docker --build-arg CACHEBUST=$(date +%Y%m%d) $SCRIPT_DIR/docker/.
docker run --name iosevka-builder iosevka-docker
docker cp iosevka-builder:/tmp/Iosevka/dist/iosevka-custom/ $SCRIPT_DIR/out/
docker stop iosevka-builder
docker rm iosevka-builder
