#!/usr/bin/env bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

rm ~/.bashrc 2> /dev/null
ln -s $DIR/bashrc ~/.bashrc

rm ~/.inputrc 2> /dev/null
ln -s $DIR/inputrc ~/.inputrc

rm ~/.tmux.conf 2> /dev/null
ln -s $DIR/tmux.conf ~/.tmux.conf

rm ~/.Xdefaults 2> /dev/null
ln -s $DIR/Xdefaults ~/.Xdefaults

rm ~/.vimrc 2> /dev/null
ln -s $DIR/vimrc ~/.vimrc

rm ~/.muttrc 2> /dev/null
ln -s $DIR/muttrc ~/.muttrc

rm ~/.offlineimaprc 2> /dev/null
ln -s $DIR/offlineimaprc ~/.offlineimaprc

rm ~/.gdbinit 2> /dev/null
ln -s $DIR/gdbinit ~/.gdbinit

rm ~/.haskeline 2> /dev/null
ln -s $DIR/haskeline ~/.haskeline

rm ~/.ghci 2> /dev/null
ln -s $DIR/ghci ~/.ghci

rm ~/.stack/config.yaml 2> /dev/null
ln -s $DIR/stack/config.yaml ~/.stack/config.yaml
