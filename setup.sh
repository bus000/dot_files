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

rm -R ~/.xmonad 2> /dev/null
ln -s $DIR/xmonad ~/.xmonad

rm ~/.vimrc 2> /dev/null
ln -s $DIR/vimrc ~/.vimrc

rm ~/.xinitrc 2> /dev/null
ln -s $DIR/xinitrc ~/.xinitrc

rm ~/.xsessionrc 2> /dev/null
ln -s $DIR/xsessionrc ~/.xsessionrc

rm ~/.xmobarrc 2> /dev/null
ln -s $DIR/xmobarrc ~/.xmobarrc
