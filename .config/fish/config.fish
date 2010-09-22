
# prompt now defined as a function on /functions. Using the git branch thing
# function fish_prompt -d "Write out the prompt"
# 	printf '%s@%s%s\n%s> ' (whoami) (set_color $fish_color_cwd) (pwd) (set_color normal)
# end

# make vim ! commands call the sh shell, not fish
function vim -d 'Vi improved'
    set old_shell $SHELL
    set SHELL /bin/sh
    command vim $argv
    set SHELL $old_shell
end

# needed for OO.org to work on xmonad
set SAL_USE_VCLPLUGIN gen


set R_HISTFILE ~/.Rhistory
set HGEDITOR /usr/bin/vim.gnome
set EDITOR /usr/bin/vim.gnome
# set env variables
set PATH /home/quesada/bin/bin /sbin /usr/local/bin /usr/bin /bin /opt/bin /usr/x86_64-pc-linux-gnu/gcc-bin/4.4.2 /home/quesada/bin	/home/quesada/wuala

# fish vars

set XDG_CONFIG_HOME $HOME/.config
#set fish_function_path $fish_function_path $XDG_CONFIG_HOME/fish/functions $XDG_CONFIG_HOME/fish/completions

set EDITOR /usr/bin/vim
set PAGER /usr/bin/most
set PYTHONSTARTUP /home/quesada/.pystartup
set MOZ_DISABLE_PANGO 1
#set SCALA_HOME /home/quesada/prgmmnLngs/lib/scala

# for virtualenv, but it doesn't work. only bash and zsh
#set WORKON_HOME "$HOME/.virtualenvs"
#. /usr/bin/virtualenvwrapper.sh

# aliases

function k; kate $argv&; end;
function l; less $argv; end;
function th; thunar .&; end;
function fm; pcmanfm .&; end;
function tun; exec ssh-agent bash; end;

function wri; xwrits typetime=30 finger after 6 clock multiply=5:1.4&; end;
function tra; trayer --edge top --align right --width 10 --height 18 --widthtype request  --transparent true&; end;
function fishrc;	gvim ~/.config/fish/config.fish&;	end;
function fishdot;	. ~/.config/fish/config.fish;	end;
function frc;	gvim ~/.config/fish/config.fish&;	end;
function fdt;	. ~/.config/fish/config.fish;	end;

function xmo;    gvim ~/.xmonad/xmonad.hs&;   end;
function mci;    gvim ~/.mc/ini&;   end;
function mcft;    gvim ~/.mc/bindings&;   end;
function mck;    gvim ~/.mc/mc.keymap&;   end;

function cdd;	cd $HOME/Dropbox/dotfiles;	end;

#function ack;	ack-grep;	end;
function pyc;	/home/quesada/bin/pycharm.sh&;	end;
function ..; 	cd ..; 	end;
function ...; 	cd ../..; 		end;
function cd.; 	cd ..; 		end;
function cd..; 	cd ../..; 		end;
function cd...; cd ../../..; 			end;
function du; 	du -h $argv; 		end;
function df; 	df -h $argv; 		end;
#function ls; 	ls -G $argv;		end;
function la; 	ls -la $argv;		end;
function v; 	vim $argv; 		end;
function g; 	gedit $argv; 		end;
function p; 	pydoc $argv; 		end;
function gv; 	gvim $argv; 	end;
function col; xmodmap.colemak & xset r 66; end;
function qwerty; setxkbmap us; xset -r 66; end;
# equo
function euu; equo update & equo install equo entropy --relaxed & equo upgrade; end;
function eu; equo update & equo install equo entropy --relaxed; end;
function ei; equo install $argv; end;
function es; equo search $argv; end;

function pb; ssh quesada@192.168.1.20; end;
function mpi;	ssh 141.14.157.1; end;
function hiwi16; ssh 172.22.0.104; end;
function tmpi; ssh -l quesada -L 12345:172.22.0.104:22 canetoad.mpib-berlin.mpg.de; end;
function hiwi; ssh -l quesada -p 12345 localhost; end;
function hiwihome; sshfs -p 12345 quesada@localhost:/mnt/local/quesada /mnt/hiwi/; end;
function ldsr; ssh larkc@93.123.21.85; end;
# FUSE sshfs
function qhome; sshfs -p 8022 -o uid=1001,gid=1021 urlwolfpack.gotdns.org:/home/quesada ~/qdesktop/; end;
function qdata; sshfs -p 8022 -o uid=1001,gid=1021 urlwolfpack.gotdns.org:/data ~/qdata/; end;
function pdfview; /opt/cxoffice/bin/wine --bottle=office --cx-app=PDFXCview.exe&; end;

function ri -d "Alias to ri with ANSI colors"
    ri -Tf ansi $argv
end

function m
    set tmpfile ~/tmp/mcpwd-(random)
    command mc -P $tmpfile
    cd (cat $tmpfile) 2>&1
    rm $tmpfile
end

function ll
    ls -Glh	$argv
end

function unpack -d "Unpack arbitrary archive files"
    for i in $argv
        switch $i

case '**.tar'
    tar -xf $i

case '**.tar.gz' '**.tgz'
    tar -zxf $i

case '**.tar.bz' '**.tar.bz2' '**.tbz' '**.tbz2'
     tar -jxf $i

case '**.rar'
     unrar e $i

case '**.zip'
     unzip $i

case '**'
    echo File $i is of unknown type

        end
    end
end

#fix java for xmonad wmname fr m suckless)
wmname LG3D

#if status --is-login
stty -xion
stty intr \^q
stty susp \^b
stty stop \^-
#end

# quake-like term
#/usr/bin/urxvt -pe kuake

#cd $HOME;
