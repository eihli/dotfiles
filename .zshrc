# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/eihli/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git tmux pyenv poetry)

source $ZSH/oh-my-zsh.sh

# User configuration

export MANPATH="/usr/local/man:/usr/share/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
export EDITOR=vim

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias ap='ansible-playbook'
alias xcp='xclip -selection clipboard'

function pyact() {
# . $HOME/.pyvenv/$1/bin/activate  # commented out by conda initialize
}

function saud () {
    # Set audio to $1
    pactl set-sink-volume 0 $1
}

[[ -f ~/.secrets ]] && . ~/.secrets

# Twitch helper

function update-todo-overlay (){
    convert -alpha on -background white -transparent white -size 800x600 -pointsize 40 caption:"$(cat $1)" /tmp/test.png
}


# Don't use .Xmodmap! https://bugs.launchpad.net/ubuntu/+source/linux/+bug/998310
# Commenting this out because it gets annoying to switch back and forth
# so maybe I'll just train msyelf to enjoy the keyboard I'm working with.
# setxkbmap -option ctrl:swapcaps

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored
zstyle :compinstall filename '/home/eihli/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=50000
SAVEHIST=100000
# End of lines configured by zsh-newuser-install

###-tns-completion-start-###
if [ -f /home/eihli/.tnsrc ]; then
    source /home/eihli/.tnsrc
fi
###-tns-completion-end-###

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /home/eihli/.local/bin/vault vault

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

if [ -z "$SSH_AUTH_SOCK" ]; then
    eval "$(ssh-agent)" > /dev/null
fi

export PATH=$PATH:/usr/local/pgsql/bin

PATH="/home/eihli/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/eihli/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/eihli/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/eihli/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/eihli/perl5"; export PERL_MM_OPT;

function penvn () {
    python3 -m venv ~/.virtualenvs/"$@"
}

function penva() {
 . ~/.virtualenvs/"$@"/bin/activate
}


gclw () {
	git clone "git@github.com:$1.git" $2
}

export PATH="$HOME/common-lisp/lem:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$PATH:/usr/local/go/bin"

#### Github Shortcuts

function gshcl () {
    dir="${2:-${1##*/}}"
    git clone git@github.com:${1}.git $dir
}

function ghhcl () {
    dir="${2:-${1##*/}}"
    git clone https://github.com/${1}.git $dir
}

function dowacom () {
    id=$(xsetwacom --list devices | head -n1 | sed -n -e 's/.*id: \([0-9]\+\).*/\1/p')
    xsetwacom --set ${id} Area 0 0 15200 8590
    xsetwacom --set ${id} MapToOutput HEAD-0
}
