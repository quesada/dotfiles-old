#- Author: "lymphatik".
#- Last updated: 2010-04-27.
#--
#- .zshrc: Sourced when a bash instance opens.
#- GREATLY inspired by gigamo's configs. http://github.com/gigamo/dotfiles
#---

#{{{ KEYS
bindkey -v #use vi keymap
bindkey "\e[A" history-beginning-search-backward #cursor up
bindkey "\e[B" history-beginning-search-forward  #cursor down
bindkey "\e[1~" beginning-of-line #home
bindkey "\e[4~" vi-end-of-line # end
bindkey "\e[8~" end-of-line
bindkey "\e[7~" vi-beginning-of-line
bindkey '\eOc' forward-word # ctrl cursor right
bindkey '\eOd' backward-word # ctrl cursor left
bindkey "^X" push-line-or-edit # push current command into a buffer, allows you to do another command then returns to previous command
bindkey ' ' magic-space

#}}}

#{{{ VARIABLES
#color in man
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export MOZ_DISABLE_PANGO=1

export EDITOR=vim
typeset -U PATH #to prevent redundancy in path

#}}}

#{{{ LOGON
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
  startx
  logout
fi

#}}}

#{{{ ALIASSES
#- Standard aliasses
alias ..='cd ..'
alias ...='cd ../..'
alias home='cd ~'
alias x='startx'
#- Verbose everything to avoid stupid mistakes
alias backup='bash /home/lymphatik/.xmonad/script/backup'
alias cp='cp -vi'
alias efind='bash /home/lymphatik/.xmonad/script/efind'
alias grep='grep -i --color=auto'
alias la='ls -lFA'
alias less=$PAGER
alias ln='ln -v'
alias lsbig='ls -lSh *(.) | head' # List biggest
alias lsd='ls -lhd *(-/DN)' #dir only
alias lsf='ls -lhA *(-.DN)' #file only
alias ls='ls -h --group-directories-first --color=auto' #sort files by size
alias lsnew='ls -lrt *(.) | tail' # List newest
alias lsold='ls -lrt *(.) | head' # List oldest
alias lssmall='ls -lSh *(.) | head' # List smallest
alias mmv='noglob zmv -W'
alias mv='mv -vi'
alias ncmpc='ncmpcpp'
alias pacgraph='pacgraph -c'
alias pacman-cage='bash /home/lymphatik/.xmonad/script/pacman-cage'
alias pacman='sudo pacman'
alias pacman-uncage='bash /home/lymphatik/.xmonad/script/pacman-uncage'
alias pac='sudo clyde -S'
alias pcc='sudo pacman -Scc'
alias pc='sudo diffpac'
alias pi='clyde -Si'
alias ping='ping -c 5'
alias pqdt='sudo pacman -Qdt'
alias pr='sudo pacman -Rcs'
alias pss='clyde'
alias pu='sudo clyde -Syu --aur'
alias rm='rm -Iv' #the -I only for recursive and if >3 files
alias svi='sudo gvim -p' #to open multiple files in tab
alias vi='gvim -p' #to open multiple files in tab
alias wget='wget --timeout 10'
alias xbmc='amixer -q -c 0 set PCM 100; amixer -q -c 0 set Surround 100; amixer -q -c 0 set Center 100; amixer -q -c 0 set LFE 100;xbmc;amixer -q -c 0 set PCM 10; amixer -q -c 0 set Surround 10; amixer -q -c 0 set Center 10; amixer -q -c 0 set LFE 10'


#- Global aliasses
alias -g G='| grep'
alias -g H='| head'
alias -g H='--help'
alias -g L='| less'
alias -g M='| more'
alias -g T='| tail'

#- Auto open certain files
alias -s {mpg,mpeg,avi,ogm,wmv,m4v,mp4,mov,mkv}='mplayer'
alias -s {mp3,ogg,wav,flac}='mplayer'
alias -s {html,php,com,net,org,gov,be}='firefox'
alias -s txt='gvim'
alias -s pdf='apvlv'
alias -s {rar,zip,7z,tar.bz2.gz}='aunpack'

# ls settings
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=01;33:so=01;35:do=01;35:bd=00;33;01:cd=00;33;01:or=05;31;01:ex=01;37:*.tar=00;32:*.tgz=00;32:*.arj=00;32:*.taz=00;32:*.lzh=00;32:*.zip=00;32:*.z=01;32:*.Z=01;32:*.gz=01;32:*.bz2=01;32:*.jar=01;32:*.rar=01;32:*.ace=01;32:*.7z=01;32:*.jpg=00;36:*.jpeg=00;36:*.gif=00;36:*.bmp=00;36:*.pbm=00;36:*.pgm=00;36:*.ppm=00;36:*.tga=00;36:*.xbm=00;36:*.xpm=00;36:*.tif=00;36:*.tiff=00;36:*.png=00;36:*.svg=00;36:*.svgz=00;36:*.mng=00;36:*.pcx=00;36:*.nuv=00;36:*.flc=00;36:*.fli=00;36:*.flv=00;36:*.gl=00;36:*.dl=00;36:*.xcf=00;36:*.xwd=00;36:*.yuv=00;36:*.aac=00;33:*.au=00;33:*.flac=00;33:*.mid=00;33:*.midi=00;33:*.mka=00;33:*.mp3=00;33:*.mpc=00;33:*.ogg=00;33:*.ra=00;33:*.wav=00;33:*.mov=01;36:*.mpg=01;36:*.mpeg=01;36:*.m2v=01;36:*.mkv=01;36:*.ogm=01;36:*.mp4=01;36:*.m4v=01;36:*.mp4v=01;36:*.vob=01;36:*.qt=01;36:*.wmv=01;36:*.asf=01;36:*.rm=01;36:*.rmvb=01;36:*.avi=01;36';

# watch=(notme)               # watch login/logout with the command log

#}}}

#{{{ FUNCTIONS
canal()
{
    date=`date +%y%m%d --date ''0' days ago'`
    PAGE="http://www.canalplus.fr/rest/bootstrap.php?/bigplayer/search/$1"
    VIDEO=$( wget -q -O - $PAGE | grep -o -E 'rtmp[^<]*L.flv' | grep $date | grep $1 )
    echo "$VIDEO"
    i=0
    if [ ! -z "$VIDEO" ]
        then
        for link in $VIDEO
            do
                let "i=i+1"
                flvstreamer -r $link -o $1"$i".mkv
            done
        mplayer -fs -speed 1.6 $1[0-9].mkv
        rm $1[0-9].mkv
    else
        echo "Pas de vidéo trouvée pour ce jour"
    fi
}

merge() {
  if [ $# -lt 3 ]; then
    echo "usage: merge Movie Movie-CD1 Movie-CD2"
  else
     mencoder -ovc copy -oac copy -o "$1" "$2" "$3";
  fi
}

up.rm.m() {
  if [ $# -lt 2 ]; then
    echo "usage: up.rm.m Movie time"
  else
      mencoder -ovc copy -oac copy -ss 0 -endpos "$2" -o temp.avi "$1"
      rm "$1"
      mv temp.avi "$1"
  fi
}

up.rm.s() {
  if [ $# -lt 1 ]; then
    echo "usage: up.rm.s TVShow"
  else
      LEN=`mplayer -vo null -ao null -frames 0 -identify "$1" 2>/dev/null | grep "^ID_LENGTH" | sed -e 's/ID_LENGTH=//g'`
      temps=10
      temp=$(echo "$LEN-$temps" | bc)
      mencoder -ovc copy -oac copy -ss 0 -endpos $temp -o temp.avi "$1"
      rm "$1"
      mv temp.avi "$1"
  fi
}

up.tl() {
    find -iname "*.rar" -exec aunpack {} "*.avi" \;
# find -atime -1 -print0 | xargs -0 -I file $(up.rm.s file)
    tvrenamer.pl
    rm *.rar
}

#- create playlist
playlist() {
    IFS=$'\n'
    playfile="/home/lymphatik/playlist";
        case $1 in
            -tv) find /backup/TV -type f -newerma $playfile.tv >> $playfile.tv;;
            -an) find /backup/Anime -type f -newerma $playfile.anime >> $playfile.anime;;
           -mov) find /backup/Movies -type f -newerma $playfile.movie >> $playfile.movie;;
            -ptv) mplayer -fs -playlist $playfile.tv;;
            -pan) mplayer -fs -playlist $playfile.anime;;
            -pmov) mplayer -fs -playlist $playfile.movie;;
            *) echo "$1 $2 Learn how to read bash script";;
        esac
}


#- Search by paragraph
grepp() {
    if test -z "$1" || test -z "$2" ; then
        echo "USAGE: grepp searchterm filetosearch";
    else
        perl -00ne "print if /$1/i" < $2
    fi
}
#- Commit conf files easily
commit() {
    git commit -a -m "$*"
    git push origin master
}

#- Extract archive.
#extract() {
#    if [ -f "$1" ] ; then
#        case "$1" in
#            *.tar.bz2) tar xvjf "$1" ;;
#            *.tar.gz) tar xzvf "$1" ;;
#            *.tar.Z) tar xzvf "$1" ;;
#            *.bz2) bunzip2 "$1" ;;
#            *.rar) unrar x "$1" ;;
#            *.gz) gunzip "$1" ;;
#            *.jar) unzip "$1" ;;
#            *.tar) tar xvf "$1" ;;
#            *.tbz2) tar xvjf "$1" ;;
#            *.tgz) tar xvzf "$1" ;;
#            *.zip) unzip "$1" ;;
#            *.Z) uncompress "$1" ;;
#            *.7z) 7z x "$1" ;;
#            *) echo "'$1' Error. Please go away" ;;
#        esac
#    else
#        echo "'$1' is not a file"
#    fi
#}

#- Manage services
service() {
  if [ $# -lt 2 ]; then
    echo "usage: service [service] [stop|start|restart]"
  else
    sudo /etc/rc.d/$1 $2
  fi
}

#- Reload .zshrc.
src() {
    autoload -U zrecompile
    [[ -f ~/.zshrc ]] && zrecompile -p ~/.zshrc
    [[ -f ~/.zcompdump ]] && zrecompile -p ~/.zcompdump
    [[ -f ~/.zcompdump ]] && zrecompile -p ~/.zcompdump
    [[ -f ~/.zshrc.zwc.old ]] && rm -f ~/.zshrc.zwc.old
    [[ -f ~/.zcompdump.zwc.old ]] && rm -f ~/.zcompdump.zwc.old
    source ~/.zshrc
}

#- Create archive.
roll() {
    if [ -n "$1" ] ; then
        FILE=$1
        case $FILE in
            *.tar.bz2) shift && tar cjf $FILE $* ;;
            *.tar.gz) shift && tar czf $FILE $* ;;
            *.tgz) shift && tar czf $FILE $* ;;
            *.zip) shift && zip $FILE $* ;;
            *.rar) shift && rar $FILE $* ;;
        esac
    else
        echo "You must specify a filename."
    fi
}

#- Define a word - USAGE: define dog
define() {
    lynx -dump "http://www.google.com/search?hl=en&q=define%3A+${1}&btnG=Google+Search" | grep -m 3 -w "*"  | sed 's/;/ -/g' | cut -d- -f1 > /tmp/templookup.txt
    if [[ -s  /tmp/templookup.txt ]] ;then
        until ! read response
        do
            echo "${response}"
        done < /tmp/templookup.txt
    else
        echo "Sorry, I can't find the term \"${1} \""
    fi
    rm -f /tmp/templookup.txt > /dev/null
}

#- Determine a directory sizes
dirsize() {
    du -shx * .[a-zA-Z0-9_]* 2> /dev/null | \
    egrep '^ *[0-9.]*[MG]' | sort -n > /tmp/list
    egrep '^ *[0-9.]*M' /tmp/list
    egrep '^ *[0-9.]*G' /tmp/list
    rm -rf /tmp/list &> /dev/null
}

#- Swap two filenames around
swap() {
    if [ $# -ne 2 ]; then
        echo "Swap: 2 arguments needed"; return 1
    fi
    if [ ! -e $1 ]; then
        echo "Swap: $1 does not exist"; return 1
    fi
    if [ ! -e $2 ]; then
        echo "Swap: $2 does not exist"; return 1
    fi
    local TMPFILE=tmp.$$ ; mv $1 $TMPFILE ; mv $2 $1 ; mv $TMPFILE $2
}

goto() { [ -d "$1" ] && cd "$1" || cd "$(dirname "$1")"; }
#- Copy and follow
cpf() { cp "$@" && goto "$_"; }
#- Move and follow
mvf() { mv "$@" && goto "$_"; }

#}}}

#{{{ PROMPT

#change the title of urxvt based on the cmd
case $TERM in
    *xterm*|rxvt|rxvt-unicode|rxvt-256color|(dt|k|E)term)
        precmd () { print -Pn "\e]0;$TERM - [%n@%M]%# [%~]\a" }
        preexec () { print -Pn "\e]0;$TERM - [%n@%M]%# [%~] ($1)\a" }
    ;;
    screen)
        precmd () {
            print -Pn "\e]83;title \"$1\"\a"
            print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a"
        }
        preexec () {
            print -Pn "\e]83;title \"$1\"\a"
            print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a"
        }
    ;;
esac

#set prompt colors based on the users and chroot
setprompt () {
    autoload -U colors zsh/terminfo
    colors
    setopt prompt_subst

    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
        eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
        eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    done
    PR_NO_COLOR="%{$terminfo[sgr0]%}"
    PR_USER="%n"
    PR_DATE="%T"
    PR_DIR="%~"

    UC=$PR_CYAN             #- User's color
    [ $UID -eq "0" ] && UC=$PR_RED  #- Root's color

    if [ -e /chroot32 ]; then
        CHROOT=" ${PR_RED}(chroot32)${PR_NO_COLOR}"
    fi

    PROMPT="${UC}${PR_USER}$CHROOT ${PR_BLUE}${PR_DIR} ${UC}»${PR_NO_COLOR} "
    RPROMPT="${UC}${PR_NO_COLOR} ${PR_DATE}"
    PS2=" ${UC}»${PR_NO_COLOR} "
}

setprompt

#}}}

#{{{ COMPLETION SYSTEM
#- Expansion options.
zstyle ':completion:*' completer _expand _complete _prefix _correct _approximate
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete

#- Completion caching.
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.cache/zsh/$HOST

#- Expand partial paths.
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh

#- Include non-hidden directories in globbed file completions
#- for certain commands.
zstyle ':completion::complete:*' '\'

#- Tag-order 'globbed-files directories' all-files.
zstyle ':completion::complete:*:tar:directories' file-patterns '*~.*(-/)'

# completion with globbing
zstyle ':completion:*' glob 'yes'

zstyle ':completion:*' insert-tab pending # tab, don't annoy me

#- Don't complete backup files as executables.
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'
zstyle ':completion:*:-command-:*:'    verbose false

#- Ignore completions for commands that we dont have
zstyle ':completion:*:functions' ignored-patterns '_*'

#- Separate matches into groups.
zstyle ':completion:*:matches' group 'yes'

#- Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

#- Messages/warnings format.
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'

#- Describe options in full.
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

#- Complete manual by their section.
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true

#- activate color-completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

#- on processes completion complete all user processes
zstyle ':completion:*:processes' command 'ps -au$USER'

#- provide more processes in completion of programs like killall
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

#- completing process IDs with menu selection
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*'   force-list always
zstyle ':completion:*:*:pkill:*' menu yes select
zstyle ':completion:*:pkill:*'   force-list always

#- ignore duplicate entries
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' stop yes

# activate menu
#zstyle ':completion:*:correct:*' insert-unambiguous true # start menu completion only if it could find no unambiguous initial string
zstyle ':completion:*:man:*' menu yes select
zstyle ':completion:*:history-words' menu yes # activate menu
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select # complete 'cd -<tab>' with menu
zstyle ':completion:*' menu select=5 yes

zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename "$HOME/.zshrc"

#- Prevent re-suggestion
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:scp:*' ignore-line yes
zstyle ':completion:*:ls:*' ignore-line yes

#}}}

#{{{ OPTIONS
#- History.
HISTSIZE=2000
SAVEHIST=2000
HISTFILE=~/.zshhistory

#- Load modules.
autoload -U compinit
compinit 2>/dev/null
autoload zmv        # For renaming.
autoload -U zcalc   # For math.
autoload -U zsh-mime-setup
zsh-mime-setup

compdef _pacman clyde=pacman

#- setopt (see http://zsh.sourceforge.net/Doc/Release/zsh_15.html)
setopt noalwaystoend          # When complete from middle, move cursor
setopt autocd               # automatically cd to paths
setopt automenu             # Automatically use menu completion after the second consecutive request for completion
setopt autopushd            # Automatically append dirs to the push/pop list
setopt BANG_HIST
setopt banghist             # Perform textual history expansion, csh-style, treating the character `!' specially.
setopt cdablevars           # Avoid the need for an explicit $
setopt completeinword       # Not just at the end
setopt correctall           # spelling correction
setopt extendedglob         # Weird & wacky pattern matching - yay zsh!
setopt EXTENDED_HISTORY
setopt globdots             # Include dot file in globbing
setopt HIST_FIND_NO_DUPS
setopt hist_ignore_all_dups # when I run a command several times, only store one
setopt HIST_IGNORE_DUPS
setopt hist_ignore_space    # don't store command which start by a space
setopt HIST_NO_STORE        # don't save 'history' cmd in history
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt histverify           # When using ! cmds, confirm first
# setopt ignoreeof            # forces the user to type exit or logout, instead of just pressing ^D
setopt INC_APPEND_HISTORY
setopt interactivecomments  # turns on interactive comments; comments begin with a #
setopt LONG_LIST_JOBS       # display PID when suspending processes as well
setopt monitor              # job control
setopt multios              # Perform implicit tees or cats when multiple redirections are attempted
setopt NO_AUTO_REMOVE_SLASH # When the last character resulting from a completion is a slash and the next character typed is a word delimiter, a slash, or a character that ends a command (such as a semicolon or an ampersand), remove the slash.
setopt nobeep               # who loves thos nasty beeps
setopt noclobber            # prevents you from accidentally overwriting an existing file. If you really do want to clobber a file, you can use the >! operator
setopt nohup                # and don't kill them, either
setopt listtypes            # Show types in completion
setopt nopromptcr           # Don't add \n which overwrites cmds with no \n
setopt NO_SH_WORD_SPLIT     # use zsh style word splitting
setopt notify               # notify immediately, not on next prompt
setopt printexitvalue       # alert me if something's failed
setopt pushdignoredups      # And don't duplicate them
setopt pushdminus
setopt pushdsilent
setopt pushdtohome
setopt SHARE_HISTORY
setopt vi                   # use bindkeys

#}}}

#{{{ WIDGET

# open ranger in the current directory by using Ctrl+f
integrate_ranger()
{
     ranger <$TTY
     cd "$(grep \^\' ~/.ranger/bookmarks | cut -b3-)"
     precmd
     zle redisplay
}
zle -N integrated-ranger integrate_ranger
bindkey '^E' integrated-ranger

# change the cursor color to red when in cmd mode vim style and grey when in insert mode
zle-keymap-select () {
if [ $KEYMAP = vicmd ]; then
echo -ne "\033]12;Red\007"
else
echo -ne "\033]12;Grey\007"
fi
}
zle -N zle-keymap-select
zle-line-init () {
zle -K viins
echo -ne "\033]12;Grey\007"
}
zle -N zle-line-init
bindkey -v

#}}}
