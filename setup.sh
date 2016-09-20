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

# Configure background photo.
sudo rm /usr/share/backgrounds/background.png 2> /dev/null
sudo cp ./background.png /usr/share/backgrounds/background.png
sudo chmod a+r /usr/share/backgrounds/background.png

# Configure a quote to be written when logging in.
sudo rm -R ./quotes 2> /dev/null
git clone https://github.com/bus000/quotes
echo '# Print a random quote when the terminal opens only if the parent is not vim.' >> bashrc
echo 'export PATH=$PATH:'$DIR'/quotes/' >> bashrc
echo 'if [ $PARENT_NAME != 'vim' ]; then' >> bashrc
echo '    randomquote.py | cowthink' >> bashrc
echo 'fi' >> bashrc
