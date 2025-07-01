
abbr gc "git commit"
abbr gcl "git clone"
abbr gp "git push"
abbr gpf "git push --force-with-lease"
abbr gl "git pull"
abbr gd "git diff"
abbr gds "git diff --staged"
abbr glg "git log"
abbr glp "git log -p"
abbr gco "git checkout"
abbr glpg "git checkout -p -G"
abbr gpsup "git push --set-upstream origin (git rev-parse --abbrev-ref HEAD)"
abbr grhh "git reset --hard"
abbr gst "git status"
abbr ga "git add"
abbr xcp "xclip -selection c"

set -gx XDG_STATE_HOME $HOME/.local/state
set -gx VIRTUALENV_DIR $XDG_STATE_HOME
set -gx EDITOR vim

if status is-interactive
    # Commands to run in interactive sessions can go here
end

if not set -q SSH_AUTH_SOCK
    eval (ssh-agent -c)
    ssh-add -K
end

pyenv init - fish | source
