# Source secrets that aren't committed to repo
[[ -f ~/.secrets ]] && . ~/.secrets

# Source git completion
[[ -f ~/.git-completion.bash ]] && . ~/.git-completion.bash

export EDITOR="emacs -nw"
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;35m\]\W\[\033[00m\]\$ '

# TODO: Move aliases to .bash_aliases
# http://askubuntu.com/questions/121413/understanding-bashrc-and-bash-profile
# Meta alias's
alias prof="nvim ~/.bash_profile"
alias reprof=". ~/.bash_profile"

# command line aliases
alias ls='ls -GFh'
alias lsa='ls -GFha'

# git alias
alias gm="git merge"
alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gcl="git clone"
alias gch="git checkout"
alias gl="git log --oneline"
alias gps="git push"
alias gpl="git pull"
alias gb="git branch"
alias gd="git diff"
alias grc="git rebase --continue"
alias gpr="git pull --rebase upstream master"
alias glg="git log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short"
alias gbd="git branch -d"
alias gpssu="git push --set-upstream"
function ghcr() {
  curl -u 'eihli' https://api.github.com/user/repos -d '{"name": "'$1'"}'
  git remote add origin https://github.com/eihli/$1
}

# Git functions
function grp() {
  branch_name="$(git symbolic-ref HEAD 2>/dev/null)" ||
  branch_name="(unnamed branch)"
  branch_name=${branch_name##refs/heads/}
  git pull --rebase upstream master && git push origin $branch_name
}
function gitpr() {
  # gitpr <remote repo> <pull request number> <new branch name>
  # fetches the PR and stores it in branchname so you
  # can view it locally
  REMOTE=$1
  ID=$2
  BRANCHNAME=$3
  git fetch $REMOTE pull/$ID/head:$BRANCHNAME
  git checkout $BRANCHNAME
}

# productivity aliases
alias sc="cd ~/code/scratch"
alias gp="cd ~/Projects/grouper"

# docker
alias bd="boot2docker"
alias dk="docker"
alias doc="docker-compose"
alias dkrmi='docker rmi $(docker images | grep "^<none>" | awk "{print $3}")'
# alias dockerrma="docker rm $(docker ps -a -q)"

# productivity functions
mcd() { mkdir $1 && cd $1; }

function gts() {
  local folder=$1
  local url="https://github.com/eihli/test-skeleton"
  git clone $url $folder
  cd $folder
}

function mktest() {
  moduleName=$1
  wget https://github.com/eihli/test-skeleton/archive/0.9.zip -O temp.zip
  unzip temp.zip -d ./
  mv ./test-skeleton*/* ./
  rm -r ./test-skeleton* temp.zip
  npm install
  grunt watch
}

function toggle-utc() {
  touch ~/.timezone_info
  PREV=$(echo $(ls -al /etc/localtime | awk '{print $11}'))
  echo 'Previous timezone was: '$PREV
  NEW=$(cat ~/.timezone_info)
  if [ -z $NEW ]; then
    echo '/usr/share/zoneinfo/UTC' >> ~/.timezone_info
    NEW=$(cat ~/.timezone_info)
  fi
  echo 'New timezone is: '$NEW
  sudo ln -sf $NEW /etc/localtime && echo $PREV > ~/.timezone_info
}

# If login shell, source bashrc
case $- in *i*) . ~/.bashrc;; esac

which pyenv &> /dev/null && eval "$(pyenv init -)"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
[[ -f ~/.fh_bash_profile ]] && . ~/.fh_bash_profile
