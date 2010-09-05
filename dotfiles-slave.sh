#!/bin/bash
# this is to be run at $HOME
DOTDIR='/home/quesada/dotfiles'
mkdir -p $DOTDIR

# not using a loop since we may want to comment out parts
# dirs
#cd $HOME
#mkdir -p $DOTDIR/.config
#rm $HOME/.config/xfce4 $DOTDIR/.config
#cd $HOME/.config
#ln -s $DOTDIR/.config/xfce4 $HOME/.config/

# all others are in home so:
cd $HOME
rm $HOME/.fonts
ln -s $DOTDIR/.fonts

rm $HOME/.kde4
ln -s $DOTDIR/.kde4

rm $HOME/.mc
ln -s $DOTDIR/.mc

rm $HOME/.xmonad
ln -s $DOTDIR/.xmonad

rm $HOME/.lyx
ln -s $DOTDIR/.lyx

rm $HOME/.weechat
ln -s $DOTDIR/.weechat


rm $HOME/.workrave
ln -s $DOTDIR/.workrave


rm $HOME/.task
ln -s $DOTDIR/.task

rm -r $HOME/.config/fish
ln -s $DOTDIR/.config/fish $HOME/.config

# files
rm $HOME/.Xdefaults
ln -s $DOTDIR/.Xdefaults

rm $HOME/.bashrc
ln -s $DOTDIR/.bashrc

rm $HOME/.cola
ln -s $DOTDIR/.cola

rm $HOME/.gitconfig
ln -s $DOTDIR/.gitconfig

rm $HOME/.hgrc
ln -s $DOTDIR/.hgrc

rm $HOME/.vimperatorrc
ln -s $DOTDIR/.vimperatorrc

rm $HOME/.vimrc
ln -s $DOTDIR/.vimrc

rm $HOME/.xinitrc
ln -s $DOTDIR/.xinitrc

rm $HOME/.xmobarrc
ln -s $DOTDIR/.xmobarrc

rm $HOME/.zshrc
ln -s $DOTDIR/.zshrc
