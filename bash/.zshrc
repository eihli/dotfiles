# Bootstrap PATH for macOS (Homebrew, MacPorts) and Linux
path=(
    $HOME/.local/bin
    $HOME/bin
    /opt/homebrew/bin      # macOS Homebrew (Apple Silicon)
    /opt/homebrew/sbin
    /usr/local/bin         # macOS Homebrew (Intel) / Linux
    /usr/local/sbin
    /opt/local/bin         # macOS MacPorts
    /opt/local/sbin
    $path
)
typeset -U path  # dedupe

# If fish is available and we're interactive, switch to fish
if [[ -o interactive ]] && command -v fish &>/dev/null; then
    exec fish
fi

# Fallback: stay in zsh with mise + starship
if command -v mise &>/dev/null; then
    eval "$(mise activate zsh)"
fi
if command -v starship &>/dev/null; then
    eval "$(starship init zsh)"
fi
