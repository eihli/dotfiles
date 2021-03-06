# I think ansible or NVM is goofing with my MANPATH
# so source it first then reset MANPATH
[[ -d $HOME/src/ansible/hacking ]] && . $HOME/src/ansible/hacking/env-setup -q
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

export MANPATH="/usr/local/man:/usr/share/man:$MANPATH"
export ANDROID_SDK_ROOT="/opt/android-sdk"
export ANDROID_HOME="/opt/android-sdk"
export PYENV_ROOT="$HOME/.pyenv"

# Only set these if we're not in tmux. If we're in tmux,
# they've already been set.
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools

which go &> /dev/null && eval "$(go env)"
which go &> /dev/null && [[ -d "$GOPATH/bin" ]] && PATH=$PATH:$GOPATH/bin
export PATH="$PATH:$HOME/.local/bin:$HOME/bin"
which pyenv &> /dev/null && eval "$(pyenv init -)"

export EDITOR="emacs"
