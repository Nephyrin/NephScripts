[ -z "$BASH_PROFILED" ] && source /etc/profile
unset BASH_PROFILED

export PATH=$HOME/cmd:$PATH

# BenQ left monitor
export __GL_SYNC_DISPLAY_DEVICE="DFP-2"

[ -z "$XAUTHORITY" ] && export XAUTHORITY=$HOME/.Xauthority

export CCACHE_DIR=$HOME/.ccache

# chroot + sudo causes bash to be started as us but with EUID 0
# - so just drop down to a normal shell when that happens (then exit)
if [[ ${EUID} == 0 ]]; then
su - nephyrin
exit 0
fi

qr()
{
    derp=$(mktemp)
    qrencode -o "$derp" "$*"
    ( (
        gwenview "$derp"
        rm "$derp"
    )& ) &>/dev/null
}

stoparch32()
{
    # Stop mounts
    sudo umount /mnt/N/arch32/proc
    sudo umount /mnt/N/arch32/dev
    sudo umount /mnt/N/arch32/sys
    sudo umount /mnt/N/arch32/home
    sudo umount /mnt/N/arch32/etc/chroot_host
    sudo umount /mnt/N/arch32/mnt/N
}

arch32()
{
    if [ ! -d /mnt/N/arch32/home/nephyrin ]; then
        # Start mounts
        sudo mount /mnt/N/arch32/proc
        sudo mount /mnt/N/arch32/dev
        sudo mount /mnt/N/arch32/sys
        sudo mount /mnt/N/arch32/home
        sudo mount /mnt/N/arch32/mnt/N
        sudo mount /mnt/N/arch32/etc/chroot_host
        sudo mount /mnt/N/arch32/etc/chroot_host -o remount,ro
    fi
    sudo linux32 chroot /mnt/N/arch32 /bin/bash -c "su - nephyrin"
}

resteam()
{
    for x in {Bru,}Steam; do x ~/Launch/$x steam://nav/console; done
}

rand32()
{
    echo -n $(( $RANDOM << 17 | $RANDOM << 2 | $RANDOM & 0x3 )); 
}
rand64()
{
    echo -n $(( ($RANDOM << 49) | ($RANDOM << 34) | ($RANDOM << 19) | ($RANDOM << 4) | ($RANDOM & 15 ) ));
}
# Max positive range
rand63()
{
    echo -n $(( $(rand64) & ~(1 << 63) ));
}

dcg()
{
    /bin/echo $$ > /sys/fs/cgroup/tasks
    unset NEPH_CGROUP
    unset NEPH_CGROUP_PS1
}

cg()
{
    local shelldir
    local i
    
    while [ -d /sys/fs/cgroup/$shelldir ] && i=$(( $i + 1 )); do
        shelldir=shell$$:$i
    done
    
    mkdir /sys/fs/cgroup/$shelldir
    /bin/echo $$ > /sys/fs/cgroup/$shelldir/tasks
    /bin/echo $$ > /sys/fs/cgroup/$shelldir/tasks
    export NEPH_CGROUP=$shelldir
    export NEPH_CGROUP_PS1="$(echo -e "\033[0;37m$shelldir ")"
}

java_memanalyze()
{
    if [ -z "$1" ]; then
        jps
    else
        pid=$1
        (
            file=$(mktemp -u)
            trap "rm -vf \"$file\"" EXIT
            echo ":: Dumping process memory"
            jmap "-dump:format=b,file=$file" $pid
            echo ":: Dump complete, detached from process. Analyzing."
            jhat -J-Xmx4096M "$file"
        )
    fi
}

irctun() { ssh -T -f -L17079:127.0.0.1:17079 jschoenick@people.mozilla.org sleep 30 && echo ":: 30s to start IRC connection"; }

alias rv='rsync -avy --partial --progress'

#if [ "$PS1" ] ; then  
#   mkdir -m 0700 /sys/fs/cgroup/user/$$
#   echo $$ > /sys/fs/cgroup/user/$$/tasks
#fi

# OR I COULD JUST PROPERLY FIX IT LOL
function fixraid()
{
    sudo mdadm --stop /dev/md*
    sudo mdadm --detail /dev/md*
    sudo mdadm --assemble --scan
    sudo mount /mnt/N
    sudo /etc/rc.d/deluged restart
    killall gnome-panel
}


if [[ $- == *i* ]] ; then
    # Interactive shell options
    # Bash won't get SIGWINCH if another process is in the foreground.
    # Enable checkwinsize so that bash will check the terminal size when
    # it regains control.  #65623
    # http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
    shopt -s checkwinsize
    shopt -s histappend
    
    # Change the window title of X terminals 
    case ${TERM} in
        xterm*|rxvt*|Eterm|aterm|kterm|gnome*|interix)
            PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
            ;;
        screen)
            PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
            ;;
    esac
    use_color=false

    # Set colorful PS1 only on colorful terminals.
    # dircolors --print-database uses its own built-in database
    # instead of using /etc/DIR_COLORS.  Try to use the external file
    # first to take advantage of user additions.  Use internal bash
    # globbing instead of external grep binary.
    safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
    match_lhs=""
    [[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
    [[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
    [[ -z ${match_lhs}    ]] \
        && type -P dircolors >/dev/null \
        && match_lhs=$(dircolors --print-database)
    [[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

    if ${use_color} ; then
        # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
        if type -P dircolors >/dev/null ; then
            if [[ -f ~/.dir_colors ]] ; then
                eval $(dircolors -b ~/.dir_colors)
            elif [[ -f /etc/DIR_COLORS ]] ; then
                eval $(dircolors -b /etc/DIR_COLORS)
            fi
        fi

        if [[ ${EUID} == 0 ]] ; then
            PS1='$NEPH_CGROUP_PS1\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
        else
            PS1='$NEPH_CGROUP_PS1\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
        fi

        alias ls='ls --color=auto'
        alias grep='grep --colour=auto'
    else
        if [[ ${EUID} == 0 ]] ; then
            # show root@ when we don't have colors
            PS1='\u@\h \W \$ '
        else
            PS1='\u@\h \w \$ '
        fi
    fi

    # Try to keep environment pollution down, EPA loves us.
    unset use_color safe_term match_lhs

fi

#export CFLAGS="-march=core2 -msse4 -mcx16 -msahf -O2 -pipe -msse4.1 -msse4.2 -ftree-vectorize"
#export CXXFLAGS="${CFLAGS}"

# Shit I do alot
alias ns1="ssh srcds@174.37.110.81"
alias ns2="ssh srcds@74.55.49.243"
alias nsw="ssh apaloma@173.193.9.83"
alias ntf="ssh tf@nemu.pointysoftware.net"
alias naf="ssh tfadmin@nemu.pointysoftware.net"
alias nnf="ssh nemu.pointysoftware.net"
alias c32="sudo linux32 chroot /opt/i686_chroot/ /bin/bash"
alias iswine='ps -A | grep -Ei "wine|exe|stea|hl2"'

alias xg='x gedit'
alias xk='x kate'

export BROWSER="firefox '%s' &"
export EDITOR="nano -w"
export WINEPREFIX=/mnt/N/wine

alias 'uphctags'='ctags -RV -f ~/.ctags_heavy --c++-kinds=+p --fields=+iaS --extra=+q `cat ~/.ctags_heavylist`'
alias 'uplctags'='ctags -RV -f ~/.ctags_light --c++-kinds=+p --fields=+iaS --extra=+q `cat ~/.ctags_lightlist`'

lcg()
{
  local cgroup=$1
  if [ -z "$cgroup" ]; then
    for x in /sys/fs/cgroup/*; do
      [ -d "$x" ] && [ -f "$x"/tasks ] && lcg "$(basename "$x")"
    done
  else
    if [ ! -d /sys/fs/cgroup/"$cgroup" ]; then
      echo ":: cgroup $cgroup doesn't exist";
      return
    fi
    echo ":: Tasks for $cgroup"
    local tasks="$(cat /sys/fs/cgroup/"$cgroup"/tasks)"
    if [ -z "$tasks" ]; then
      echo " none"
    else
      ps -o pid= -o comm= -p "${tasks/$'\n'/ }"
    fi
  fi
}

alias y='yaourt'
alias p='sudo pacman'

alias dir='x dolphin "$@"'

function ct
{
    dir=`mktemp -d`
    export NEPH_TEMP_DIR="$dir"
    cd "$dir"
    touch .nephtemp
}

function clt
{
    for x in /tmp/tmp.*; do
        if [ -d "$x" ] && [ -f "$x/.nephtemp" ]; then
            if [ -z "$(fuser "$x")" ]; then
                echo ":: Removing $x"
                rm -r $x
            else
                echo ":: Skipping $x (in use)"
            fi
        fi
    done
    [ ! -z "$NEPH_TEMP_DIR" ] && [ ! -d "$NEPH_TEMP_DIR" ] && unset NEPH_TEMP_DIR
}

function rt
{
    if [ ! -z "$NEPH_TEMP_DIR" ] && [ -d "$NEPH_TEMP_DIR" ]; then
        cd "$NEPH_TEMP_DIR"
    else
        unset NEPH_TEMP_DIR
        echo ":: No temp dir in context"
    fi
}

function ipof { host "$@" | awk '{print $(NF)}'; }

alias resetswap='sudo swapoff LABEL=NephSwap && sudo swapon LABEL=NephSwap && echo ":: Done"'

function nb { nmblookup "$@" | tail -n+2 | head -n1 | grep -Eo "^[^ ]+"; }
function service { sudo /etc/rc.d/$1 $2; }

alias fixvbox='sudo vboxbuild && sudo modprobe vboxdrv && sudo modprobe vboxnetflt && sudo modprobe vboxnetadp'

if [[ -z "$NEPH_NONINTERACTIVE" ]]; then
  keyfile="~/.keychain/$(hostname)-sh"
  if which keychain &>/dev/null && [ -f "$keyfile" ] && [ -f ~/.ssh/id_rsa ]; then
    keychain ~/.ssh/id_rsa
    . "$keyfile"
  fi
  unset keyfile
  
  [ -f /proc/mdstat ] && cat /proc/mdstat
fi

say()
{
    espeak --stdout "$*" | paplay
}

function videolength
{
    ffmpeg -i "$*" 2>&1 | grep Duration | cut -d ' ' -f 4 | sed s/,//
}

function stopkde
{
    kdeinit4_shutdown
    killall kded4 klauncher kdeinit4 knotify4
}

function van
{
    yelp "man:$@" &>/dev/null &
}


x() { ("$@" >/dev/null 2>/dev/null &) }
pic() { x gwenview "$@"; }
avant() { killall avant-window-navigator; x avant-window-navigator; }

function xauthfix
{
    pid=$(pgrep -o -u nephyrin gnome-session)
    [ -z "$pid" ] && pid=$(pgrep -o -u nephyrin xfce4-session)
    [ -z "$pid" ] && pid=$(pgrep -o -u nephyrin kded4)
    if [ ! -z "$pid" ]; then
        echo "Stealing env from $pid"
        export $(cat /proc/$pid/environ | grep -z XAUTHORITY)
        else
            export XAUTHORITY=$HOME/.Xauthority
            fi
            if [ -z "$DISPLAY" ]; then export DISPLAY=:0; fi
}
    
    
function screencast_video
{
    width=$1
    height=$2
    scale=$3
    rate=$4
    fps=$5
    if [ -z "$width" ]; then width=3600; fi
    if [ -z "$height" ]; then height=1200; fi
    if [ -z "$scale" ]; then scale=0.3; fi
    if [ -z "$rate" ]; then rate=1000; fi
    if [ -z "$fps" ]; then fps=10; fi
    vlc -vvv -I dummy screen:// :screen-fps=$fps :screen-width=$width :screen-height=$height --input-slave=alsa://hw:0 :sout="#transcode{vcodec=mp4v,vb=$rate,scale=$scale,fps=$fps,hurry-up,high-priority,acodec=mp3,ac=2,ab=64,samplerate=48000}:std{access=http,mux=ts,dst=127.0.0.1:12345}"
}

function screencast_rtpv
{
    width=$1
    height=$2
    scale=$3
    rate=$4
    if [ -z "$width" ]; then width=3600; fi
    if [ -z "$height" ]; then height=1200; fi
    if [ -z "$scale" ]; then scale=0.3; fi
    if [ -z "$rate" ]; then rate=1200; fi
    vlc -vvvv -I dummy screen:// :screen-fps=10 :screen-caching=400 :screen-width=$width :screen-height=$height :sout="#transcode{venc=ffmpeg,vcodec=mp4v,vb=$rate,pre-me,scale=$scale,fps=10,hurry-up,high-priority,acodec=none}:rtp{dst=nemu.doublezen.net,port=12345,mux=ts}"
}
                                                
function screencast_rtpa
        {
vlc -vvvv -I dummy alsa://hw:0 :alsa-caching=400 :sout="#transcode{vcodec=none,acodec=mp3,ac=2,ab=64,samplerate=48000}:rtp{mux=ts,dst=nemu.doublezen.net,port=12346}"
}

function screencast_preview
{
    vlc -vvvv http://nemu.doublezen.net:12369/
}

alias screencast='(screencast_video $1 $2&screencast_audio&screencast_preview&)'
alias xderp='sudo mount -o loop /mnt/N/Applications/X-Plane/Disk1.iso /mnt/cd'

function tf2
{
    local host=$1
    local password=$2
    if [ ! -z "$host" ] && [ "$host" = "${host%.*}" ]; then
        if [ "$host" = "tf0" ]; then
            host="game.doublezen.net";
            password="dicks"
        else
            host="$1.game.doublezen.net";
        fi
    fi
    if [ ! -z "$(pidof hl2.exe)" ]; then
        echo ":: TF2 already running"
    else
        [ ! -z "$password" ] && cmdpassword="+password ${password}"
        [ ! -z "$host" ] && cmd="+connect ${host} $cmdpassword"
        c="~/Launch/Steam -applaunch 440 $cmd"
        echo "Running command $c"
        eval "$c"
    fi
}

alias rebash='source ~/.bashrc'

boost()
{
    pid=`pidof -s $1`
    for x in $(ls /proc/$pid/task); do
        # echo taskset -pc 0-7 $x
        taskset -pc 0-7 $x
        renice -10 $x
        sudo ionice -c 1 -p $x
        sudo chrt -p -f 98 $x
    done
}

replasma() { killall plasma-desktop; (plasma-desktop &>/dev/null &) }

unaff()
{
    pid=`pidof -s $1`
    for x in $(ls /proc/$pid/task); do
        taskset -p -c 0-7 $x
    done
}

alias 7zultra='7z -mx=9 -mfb=64 -md=32m -ms=on'

...() { echo ":-/"; }
