# fixes for crappy java apps misbehaving
# 1- ooffice fight for focus
#export SAL_USE_VCLPLUGIN=gen
# 2- all java apps being a grey rectangle without this:
#wmname LG3D

# connect to the web
#/usr/bin/nm-applet&

# synch with the clouds
#/home/quesada/.dropbox-dist/dropboxd&
#/home/quesada/wuala/wualacmd&
#dropbox start -i&

# tray
trayer --edge top --align right --width 10 --widthtype request --height 18 --transparent true&

# startup apps
#/usr/bin/xflux -l 52.30, 13.25&
#/usr/bin/redshift -l 52.30:13.25& # berlin
#/usr/bin/redshift -l 39.55:116.23& # beijing
#/usr/bin/workrave&
#/usr/local/bin/lyx&
/usr/bin/konsole&
/usr/bin/firefox&
easystroke&
krunner&
/usr/bin/xfce4-power-manager&
/usr/bin/xfce4-volumed&

# lappy right mouse key to win
# useful for xmonad
xmodmap -e 'keycode 135 = Super_L'
# vim map numlock to multikey to get umlauts etc #Escape
xmodmap -e 'keycode 77 = Multi_key'
# removes capslock and numlock totally
#xmodmap -e 'clear Lock'
# this disables mod2. TODO test
xmodmap -e "clear mod2"
# doesn't work: esc as caps
#xmodmap -e 'keycode 9 = Caps_Lock'

# damn bell from hell
xset b off&
# todo: if you use kdm/xdm/gdm etc you should edit the Sessions file below and
# add a line to your .initrc on your home ($HOME/.xinitrc)
#exec xmonad #already done on /etc/X11/Sessions/xmonad
