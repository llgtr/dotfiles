#!/usr/bin/env bash

set -euo pipefail

if [[ ! -e "/nix" ]]; then
    sudo mkdir -m 0755 "/nix" && sudo chown "$(whoami)" "/nix"

    curl "https://nixos.org/nix/install" | bash -s -- --no-daemon
fi
