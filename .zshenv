# I think ansible or NVM is goofing with my MANPATH
# so source it first then reset MANPATH
[[ -d $HOME/src/ansible/hacking ]] && . $HOME/src/ansible/hacking/env-setup -q
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

export MANPATH="/usr/local/man:/usr/share/man:$MANPATH"

export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export ANDROID_HOME="$HOME/Android/Sdk"
export PATH="$PATH:$HOME/.local/bin:$HOME/bin:$HOME/Android/Sdk/tools/bin:$HOME/Android/Sdk/platform-tools"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"

which go &> /dev/null && eval "$(go env)"
which go &> /dev/null && [[ -d "$GOPATH/bin" ]] && PATH=$PATH:$GOPATH/bin

export EDITOR="emacs"
