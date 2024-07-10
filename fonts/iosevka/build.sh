#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if ! docker info > /dev/null 2>&1; then
    echo "Please ensure that your container engine is running"
    exit 1
fi

docker build -t iosevka-container --build-arg CACHEBUST=$(date +%Y%m%d) .
docker run --name iosevka-builder iosevka-container
rm -rf $SCRIPT_DIR/out/
docker cp iosevka-builder:/work/dist/IosevkaFacile/ $SCRIPT_DIR/out
docker stop iosevka-builder
docker rm iosevka-builder
