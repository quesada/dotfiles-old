#!/bin/bash
# this is to be run at $HOME
DOTDIR='/home/quesada/Dropbox/dotfiles'
mkdir -p $DOTDIR

# not using a loop since we may want to comment out parts
# dirs
#cd $HOME
#mkdir -p $DOTDIR/.config
#mv $HOME/.config/xfce4 $DOTDIR/.config
#cd $HOME/.config
#ln -s $DOTDIR/.config/xfce4 $HOME/.config/

# all others are in home so:
cd $HOME
mv $HOME/.fonts $DOTDIR/
ln -s $DOTDIR/.fonts

mv $HOME/.kde4 $DOTDIR/
ln -s $DOTDIR/.kde4

mv $HOME/.mc $DOTDIR/
ln -s $DOTDIR/.mc

mv $HOME/.xmonad $DOTDIR/
ln -s $DOTDIR/.xmonad

mv $HOME/.lyx $DOTDIR/
ln -s $DOTDIR/.lyx


# files
mv $HOME/.Xdefaults $DOTDIR/
ln -s $DOTDIR/.Xdefaults

mv $HOME/.bashrc $DOTDIR/
ln -s $DOTDIR/.bashrc

mv $HOME/.cola $DOTDIR/
ln -s $DOTDIR/.cola

mv $HOME/.gitconfig $DOTDIR/
ln -s $DOTDIR/.gitconfig

mv $HOME/.hgrc $DOTDIR/
ln -s $DOTDIR/.hgrc

mv $HOME/.vimperatorrc $DOTDIR/
ln -s $DOTDIR/.vimperatorrc

mv $HOME/.vimrc $DOTDIR/
ln -s $DOTDIR/.vimrc

mv $HOME/.xinitrc $DOTDIR/
ln -s $DOTDIR/.xinitrc

mv $HOME/.xmobarrc $DOTDIR/
ln -s $DOTDIR/.xmobarrc

mv $HOME/.zshrc $DOTDIR/
ln -s $DOTDIR/.zshrc

