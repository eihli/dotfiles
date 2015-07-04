export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;35m\]\w\[\033[00m\]\$ '
# Meta alias's
alias prof="subl ~/.bash_profile"
alias reprof=". ~/.bash_profile"

# command line aliases
alias ls='ls -GFh'
alias lsa='ls -GFha'

# git alias
alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gcl="git clone"
alias gch="git checkout"
alias gl="git log --oneline"
alias gp="git push"
alias gb="git branch"
alias gd="git diff"
alias grc="git rebase --continue"
alias gpr="git pull --rebase upstream master"
alias glg="git log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short"

# Greenfield git aliases
branch_name="$(git symbolic-ref HEAD 2>/dev/null)" ||
branch_name="(unnamed branch)"
branch_name=${branch_name##refs/heads/}
alias gfp="git pull --rebase upstream master && git push origin $branch_name"

# productivity aliases
alias lg="cd ~/hackreactor/legacy"
alias sc="cd ~/code/scratch"
alias tp="cd ~/hackreactor/toy-problems"
alias td="cd ~/Projects/todoist"
alias tlm="cd ~/hackreactor/tlm"
alias selfa="cd ~/hackreactor/sa"
alias hr="cd ~/hackreactor"
alias gf="cd ~/hackreactor/greenfield"

# docker
alias bd="boot2docker"
alias dk="docker"
alias doc="docker-compose"
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
  wget https://github.com/eihli/test-skeleton/archive/master.zip -O temp.zip
  unzip temp.zip -d ./
  mv ./test-skeleton-master/* ./
  rm -r ./test-skeleton-master temp.zip
  npm install    
}

# added by Anaconda 2.1.0 installer
export PATH="/Users/eihli/anaconda/bin:$PATH"
export NODE_ENV=development
bds() { $(boot2docker shellinit); }


# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH
# bds >/dev/null 2>/dev/null
