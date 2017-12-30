# http://askubuntu.com/questions/121413/understanding-bashrc-and-bash-profile

alias emacs="emacs -nw" # No windows mode

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

# productivity aliases
alias sc="cd ~/code/scratch"
alias gp="cd ~/Projects/grouper"

# docker
alias bd="boot2docker"
alias dk="docker"
alias doc="docker-compose"
alias dkrmi='docker rmi $(docker images | grep "^<none>" | awk "{print $3}")'
# alias dockerrma="docker rm $(docker ps -a -q)"
