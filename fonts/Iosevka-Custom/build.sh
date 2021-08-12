#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if ! docker info > /dev/null 2>&1; then
    echo "Please ensure that Docker is running"
    exit 1
fi

docker build -t iosevka-docker --build-arg CACHEBUST=$(date +%Y%m%d) $SCRIPT_DIR/docker/.
docker run --name iosevka-builder iosevka-docker
rm -rf $SCRIPT_DIR/out/
docker cp iosevka-builder:/tmp/Iosevka/dist/iosevka-custom/ $SCRIPT_DIR/out/
docker stop iosevka-builder
docker rm iosevka-builder
