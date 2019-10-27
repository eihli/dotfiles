#!/bin/bash

# Step-by-step instructions for bootstrapping a new dev machine.
# This script starts with every step just being echoed to console.
# Piece-by-piece it can be automated.

confirm_step_completion () {
    read -p "$1 [y/n]: " -n 1
    echo ""
    if [[ $REPLY =~ [Yy] ]]; then
	return 0
    else
	return 1
    fi
}

if [ ! -e $HOME/.ssh/id_rsa ]; then
    echo 'Generating an ssh key with `ssh-keygen`.'
    ssh-keygen
else
    echo 'ssh key already detected'
fi

echo 'Upload the key to Github'
cat ~/.ssh/id_rsa.pub
read -p 'Press any key to continue' -n 1
echo ""

if confirm_step_completion 'Clone dotfiles repo to ~/code/dotfiles?'; then
    echo 'Clone down this repository with `git clone https://github.com/eihli/code/dotfiles`'
    mkdir -p $HOME/code/
    git clone https://github.com/eihli/dotfiles $HOME/code/dotfiles
fi

echo 'Install autotools, autogen, autoconf.'
echo 'apt install autotools-dev autogen autoconf'
read -p 'Press any key to continue' -n 1
echo ""

echo 'Install curses dev.'
echo 'apt install libncurses5-dev'
read -p 'Press any key to continue' -n 1
echo ""

echo 'Install yacc.'
echo 'apt install bison'
read -p 'Press any key to continue' -n 1
echo ""

if confirm_step_completion 'Clone and build tmux?'; then
    mkdir -p $HOME/src/
    git clone https://github.com/tmux/tmux.git $HOME/src/tmux/
    cd $HOME/src/tmux/
    sh autogen.sh
    ./configure && make
    sudo make install
fi

