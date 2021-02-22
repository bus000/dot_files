#!/usr/bin/env bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

rm ~/.bashrc 2> /dev/null
ln -s $DIR/bashrc ~/.bashrc

rm ~/.inputrc 2> /dev/null
ln -s $DIR/inputrc ~/.inputrc

rm ~/.tmux.conf 2> /dev/null
ln -s $DIR/tmux.conf ~/.tmux.conf

rm ~/.vimrc 2> /dev/null
ln -s $DIR/vimrc ~/.vimrc

rm ~/.gdbinit 2> /dev/null
ln -s $DIR/gdbinit ~/.gdbinit

rm ~/.haskeline 2> /dev/null
ln -s $DIR/haskeline ~/.haskeline

rm ~/.ghci 2> /dev/null
ln -s $DIR/ghci ~/.ghci

rm ~/.config/nvim/init.vim 2> /dev/null
ln -s $DIR/init.vim ~/.config/nvim/init.vim
