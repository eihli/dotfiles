#!/bin/bash
# Install dotfiles via stow
# Run from the dotfiles directory

set -e
cd "$(dirname "$0")"

# Install stow if missing
if ! command -v stow &> /dev/null; then
    echo "Installing stow..."
    if command -v apt &> /dev/null; then
        sudo apt install -y stow
    elif command -v brew &> /dev/null; then
        brew install stow
    else
        echo "Please install stow manually"
        exit 1
    fi
fi

# Stow all packages
for pkg in claude agents bash fish git starship tmux zellij; do
    echo "Stowing $pkg..."
    stow -t ~ "$pkg"
done

# Symlink AGENTS.md to CLAUDE.md for Claude Code
if [ -f ~/.config/AGENTS.md ] && [ ! -e ~/.claude/CLAUDE.md ]; then
    mkdir -p ~/.claude
    ln -sf ~/.config/AGENTS.md ~/.claude/CLAUDE.md
    echo "Symlinked AGENTS.md to ~/.claude/CLAUDE.md"
fi

# Set fish as default shell if not already
if [ "$SHELL" != "/usr/bin/fish" ] && grep -q /usr/bin/fish /etc/shells; then
    echo "Setting fish as default shell..."
    chsh -s /usr/bin/fish
fi

echo "Done! Symlinks created."
