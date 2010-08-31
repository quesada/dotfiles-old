# fixes for crappy java apps misbehaving
# 1- ooffice fight for focus
export SAL_USE_VCLPLUGIN=gen
# 2- all java apps being a grey rectangle without this:
wmname LG3D

# synch with the clouds
/home/quesada/.dropbox-dist/dropboxd&
/home/quesada/wuala/wualacmd&
dropbox start -i&

# tray
trayer --edge top --align right --width 10 --widthtype request --height 18 --transparent true&

# startup apps
/usr/bin/xflux -l 52.30, 13.25&
/usr/bin/workrave&
/usr/local/bin/lyx&
/usr/bin/konsole&
/usr/bin/firefox&
/usr/bin/workrave&
easystroke&
krunner&

# damn bell from hell
xset b off&
# todo: if you use kdm/xdm/gdm etc you should edit the Sessions file below and
# add a line to your .initrc on your home ($HOME/.xinitrc)
#exec xmonad #already done on /etc/X11/Sessions/xmonad
