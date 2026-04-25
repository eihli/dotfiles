# Guard needed: netbird's SSH Match exec runs through fish, sourcing all configs.
# Without this check, missing env.fish causes errors on every SSH connection.
test -f "$HOME/.cargo/env.fish" && source "$HOME/.cargo/env.fish"
