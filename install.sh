#!/bin/bash
# Install dotfiles via stow

set -e
cd "$(dirname "$0")"

# Install stow if missing
if ! command -v stow &> /dev/null; then
    echo "Installing stow..."
    sudo apt install -y stow
fi

# Stow all packages
for pkg in claude tmux agents zellij fish bash starship git; do
    echo "Stowing $pkg..."
    stow -t ~ "$pkg"
done

echo "Done! Symlinks created."
