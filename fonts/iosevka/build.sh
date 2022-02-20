#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if ! podman info > /dev/null 2>&1; then
    echo "Please ensure that podman machine is running"
    exit 1
fi

podman build -t iosevka-container --build-arg CACHEBUST=$(date +%Y%m%d) $SCRIPT_DIR/plans/.
podman run --name iosevka-builder iosevka-container
rm -rf $SCRIPT_DIR/out/
podman cp iosevka-builder:/tmp/Iosevka/dist/iosevka-term/ $SCRIPT_DIR/out
podman stop iosevka-builder
podman rm iosevka-builder
