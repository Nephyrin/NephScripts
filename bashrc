# -*- mode: sh; sh-basic-offset 2; sh-indentation 2; -*-

NEPH_DEFAULT_CGROUP=/sys/fs/cgroup/cpu

_list_contains() {
  local s
  local IFS=":"
  eval s=(\$$1)
  for x in "${s[@]}"; do
    [ "$2" != "$x" ] || return 0
  done
  return 1
}

_list_push() {
  ! _list_contains "$1" "$2" || return 0
  [ -z "$(eval echo \$$1)" ] || eval $1=":\$$1"
  eval $1="\$2\$$1"
}

_if_dir_list_push() {
  [ -d "$2" ] || return 0
  _list_push "$1" "$2"
}

_list_push PATH "$HOME/bin"
_list_push PATH "$HOME/.local/bin"
_list_push PATH "$HOME/neph/priv/bin"

export BROWSER="firefox '%s'"
export EDITOR="ec"
[ -z "$XAUTHORITY" ] && export XAUTHORITY=$HOME/.Xauthority

export CCACHE_DIR=$HOME/.ccache
export CCACHE_COMPRESS=1

# CPAN
export PERL_LOCAL_LIB_ROOT="$HOME/perl5";
export PERL_MB_OPT="--install_base $HOME/perl5";
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
export PERL5LIB="$HOME/perl5/lib/perl5/x86_64-linux-thread-multi:$HOME/perl5/lib/perl5";
_list_push PATH "$HOME/perl5/bin"

# Powerline
export PYTHONPATH
_list_push PATH "$HOME/neph/powerline/bin"
_list_push PYTHONPATH "$HOME/neph/powerline/lib/python-latest/site-packages/"

# tmux blows away TERM, so set NEPH_256COLOR_TERM if we see one, then the nested
# tmux shell can use it to see if its appropriate to update TERM
if [[ "${TERM/256color/}" != "$TERM" ]]; then
  export NEPH_256COLOR_TERM=1
elif [[ "$NEPH_256COLOR_TERM" -gt 0 && "$TERM" = "screen" && -n "$TMUX" ]]; then
  export TERM=screen-256color
fi

export MOZPATH="$HOME/moz"
export MOZDEFAULTTREE=moz-git
[ ! -f ~/bin/lib/moz.sh ] || source ~/bin/lib/moz.sh

# Machine specific
if [ "$(hostname)" = "neph" ]; then
  # BenQ left monitor
  export __GL_SYNC_DISPLAY_DEVICE="DFP-2"
  function ext() {
    env PULSE_SINK=alsa_output.pci-0000_03_00.1.hdmi-stereo-extra1 "$@"
  }
  alias tvs='pulse-sink-juggle alsa_output.pci-0000_03_00.1.hdmi-stereo-extra1 \
                               alsa_output.pci-0000_02_04.0.analog-stereo'
  # Wine is on raid array
elif [ "$(hostname)" = "Johnbook" ]; then
  #
  # Graphics switcher helpers
  #
  alias gfx_igd='sudo screen -d -m -S gfx_switch /root/live_switch.sh igd'
  alias gfx_discrete='sudo screen -d -m -S gfx_switch /root/live_switch.sh discrete'
  gfx() {
    local ret="$(cat /var/log/Xorg.0.log | grep "LoadModule: \"radeon\"")"
    if [ -z "$ret" ]; then
      echo ":: Intel Active"
    else
      echo ":: Radeon Active"
    fi
  }
fi

#
# Interactive tweaks
#
if [[ $- == *i* ]] ; then
    # Keychain
    keyfile=~/".keychain/$(hostname)-sh"
    if which keychain &>/dev/null && [ -f "$keyfile" ] && [ -f ~/.ssh/id_rsa ]; then
      keychain --nogui ~/.ssh/id_rsa
      . "$keyfile"
    fi
    unset keyfile
    [ -f /proc/mdstat ] && cat /proc/mdstat


    # Interactive shell options
    # Bash won't get SIGWINCH if another process is in the foreground.
    # Enable checkwinsize so that bash will check the terminal size when
    # it regains control.  #65623
    # http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
    shopt -s checkwinsize
    shopt -s histappend
    export PROMPT_COMMAND="history -a"

    # TMUX support
    PROMPT_COMMAND="$PROMPT_COMMAND;"'[ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD"'

    # Change the window title of X terminals
    case ${TERM} in
        xterm*|rxvt*|aterm|kterm|gnome*|interix)
            PROMPT_COMMAND="$PROMPT_COMMAND;"'echo -ne "\e]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
            ;;
        screen)
            PROMPT_COMMAND="$PROMPT_COMMAND;"'echo -ne "\e_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\e\\"'
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
        export NEPH_COLOR_TERM=1
        # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
        if type -P dircolors >/dev/null ; then
            if [[ -f ~/.dir_colors ]] ; then
                eval $(dircolors -b ~/.dir_colors)
            elif [[ -f /etc/DIR_COLORS ]] ; then
                eval $(dircolors -b /etc/DIR_COLORS)
            fi
        fi

        if [[ ${EUID} == 0 ]] ; then
            PS1='\[\e[01;31m\]\h\[\e[01;34m\] \W \$\[\e[00m\] '
        else
            PS1='\[\e[01;32m\]\u@\h\[\e[01;34m\] \w \$\[\e[00m\] '
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

    # If we're in a 32bit chroot, advertise as such
    [ "$(uname -m)" != "i686" ] || PS1="(32bit) $PS1"

    # If we're in a screen session
    [ -z "$STY" ] || PS1="\[\e[0;37m\]{\[\e[0;36m\]${STY#*.}\[\e[0;37m\]} $PS1"
fi

#
# Aliases & shorthand
#

alias pidof='pgrep -x'

alias please='eval sudo "$(fc -nl -1)"'

alias phone='sshfs root@NephGalaxy:/sdcard/ $HOME/NephGalaxy'
alias unphone='fusermount -u $HOME/NephGalaxy'

# mozilla
alias fm='ffmake'
alias fb='ffbrun'

# Hibernate-reboot with mount handling, for OS switching
alias switchos='hib reboot mounts'

alias hgreset="hg strip 'roots(outgoing())' && hg up -C && hg purge --all && hg status"
alias rv='rsync -avy --partial --progress'
alias ns1="ssh srcds@174.37.110.81"
alias ns2="ssh srcds@74.55.49.243"
alias nsw="ssh apaloma@173.193.9.83"
alias ntf="ssh tf@nemu.pointysoftware.net"
alias naf="ssh tfadmin@nemu.pointysoftware.net"
alias nnf="ssh nephyrin@nemu.pointysoftware.net"
alias relap='service laptop-mode restart'
nalb() {
  if host albus.mv.mozilla.com &>/dev/null; then
    ssh -L8000:localhost:8000 johns@albus.mv.mozilla.com "$@"
  else
    ssh -L8000:albus:8000 -t jschoenick@office.mozilla.org ssh johns@albus "$@"
  fi
}

alias c32="sudo linux32 chroot /opt/i686_chroot/ /bin/bash"
alias iswine='ps -A | grep -Ei "wine|exe|stea|hl2"'

# Random password
alias rpass='pwgen -sync 24 1'

# Git's nicer diff can be used with --no-index to diff random things
alias giff='git diff --no-index --color=auto'

alias gref='git commit --amend -a --no-edit'

alias xderp='sudo mount -o loop /mnt/N/Applications/X-Plane/Disk1.iso /mnt/cd'
alias resetswap='sudo swapoff -a && sudo swapon -a && echo ":: Done"'

alias xg='x gedit'
alias xk='x kate'
alias y='yaourt'
alias yclean='yaourt -Rs $(y -Qdtq)'
alias p='sudo pacman'

alias dir='x dolphin "$@"'

alias rebash='source ~/.bashrc'

alias brusay='ntf toybox/brusay'

#
# Misc utility functions
#

# This is necessary so that NEPH_CGROUP_PS1 and MOZ_PS1 are expanded now, such
# that \[\] escapes can be recognized, rather than literally in the prompt,
# wherien they would not be after expansion.
BASE_PS1="$PS1"
_reprompt() {
    PS1="$NEPH_CGROUP_PS1$MOZ_PS1$BASE_PS1"
}
_reprompt

# Records the current git tip in variable
gm()
{
  local var="$1"
  [ ! -z "$1" ] || var=m

  local res="$(git log -n 1 --format="%H")"
  if [ -z "$res" ]; then
    echo >&2 "!! Failed to get git revision"
  fi
  eval $var="$res"
  echo ":: $res -> $var"
}

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
    sudo umount /mnt/N/arch32/dev/pts
    sudo umount /mnt/N/arch32/dev/shm
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
        sudo mount /mnt/N/arch32/dev/shm
        sudo mount /mnt/N/arch32/dev/pts
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
    let "r = $RANDOM << 17 | $RANDOM << 2 | $RANDOM & 0x3"
    echo -n $r
}

rand64()
{
    let "r = ($RANDOM << 49) | ($RANDOM << 34) | ($RANDOM << 19) | ($RANDOM << 4) | ($RANDOM & 15 )"
    echo -n $r
}
# Max positive range
rand63()
{
    let "r = $(rand64) & ~(1 << 63)"
    echo -n $r
}

dcg()
{
    echo $$ > "$NEPH_DEFAULT_CGROUP"/cgroup.procs
    unset NEPH_CGROUP
    unset NEPH_CGROUP_PS1
    _reprompt
}

lowprio()
{
  if [ ! -z "$NEPH_CGROUP" ]; then
    echo >&2 '!! Warning, leaving old cgroup '"$NEPH_CGROUP"
    dcg
  fi
  # Create and move into new low priority cgroup
  lpcg
  # Run command with minimal nice/ionice perms
  time ionice -c3 nice -n20 "$@"
  # Delete low prio group
  dcg
}

lpcg()
{
  local group="$1"
  if [ -z "$group" ]; then
    [ -z "$NEPH_CGROUP" ] && cg
    group=$NEPH_CGROUP
  fi
  group="$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"
  [ ! -d "$group" ] && echo ":: '$group' is not a directory" && return
  /bin/echo 10 > "$NEPH_DEFAULT_CGROUP/$NEPH_CGROUP"/blkio.weight
  /bin/echo 1 > "$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"/cpu.shares
  /bin/echo 256M > "$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"/memory.soft_limit_in_bytes
  echo ":: Done"
}

# Add a process to a control group (or a new one)
pcg()
{
  local procgrep="$1"
  local cgroup="$2"
  [ -z "$procgrep" ] && echo ":: Need a taskname" && return
  local tasks=$(pgrep "$procgrep")

  [ -z "$cgroup" ] && cgroup=$(_mcg "$procgrep")
  if [ ! -d "$NEPH_DEFAULT_CGROUP"/"$cgroup" ]; then
    echo ":: cgroup $cgroup does not exist!"
    return
  fi

  for task in $tasks; do
      echo ":: Adding task $task to group '$cgroup'"
      /bin/echo $task > "$NEPH_DEFAULT_CGROUP"/"$cgroup"/cgroup.procs
  done
}

# Make a control group with prefix
mcg()
{
  # Pretty version
  local name=$(_mcg "$1")
  echo ":: Created empty control group $name"
}

_mcg()
{
  local prefix="$1"
  [ -z "$prefix" ] && echo ":: Need a name" && return
  local i
  local name
  while [ -d "$NEPH_DEFAULT_CGROUP"/"$name" ] && i=$(( $i + 1 )); do
      name="${prefix}:$i"
  done

  mkdir "$NEPH_DEFAULT_CGROUP"/"$name"

  cat "$NEPH_DEFAULT_CGROUP"/cpuset.cpus > "$NEPH_DEFAULT_CGROUP"/"$name"/cpuset.cpus
  cat "$NEPH_DEFAULT_CGROUP"/cpuset.mems > "$NEPH_DEFAULT_CGROUP"/"$name"/cpuset.mems

  echo $name
}

cg()
{
    local name=$(_mcg "shell$$")
    local i

    echo $$ > "$NEPH_DEFAULT_CGROUP"/"$name"/cgroup.procs
    export NEPH_CGROUP=$name
    NEPH_CGROUP_PS1=$'\[\e'"[0;37m\]$NEPH_CGROUP "
    _reprompt
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

# OR I COULD JUST PROPERLY FIX IT LOL
fixraid()
{
    sudo mdadm --stop /dev/md*
    sudo mdadm --detail /dev/md*
    sudo mdadm --assemble --scan
    sudo mount /mnt/N
}

lcg()
{
  local cgroup=$1
  if [ -z "$cgroup" ]; then
    for x in "$NEPH_DEFAULT_CGROUP"/*; do
      [ -d "$x" ] && [ -f "$x"/tasks ] && lcg "$(basename "$x")"
    done
  else
    if [ ! -d "$NEPH_DEFAULT_CGROUP"/"$cgroup" ]; then
      echo ":: cgroup $cgroup doesn't exist";
      return
    fi
    echo ":: Tasks for $cgroup"
    local tasks="$(cat "$NEPH_DEFAULT_CGROUP"/"$cgroup"/tasks)"
    if [ -z "$tasks" ]; then
      echo " none"
    else
      ps -o pid= -o comm= -p "${tasks/$'\n'/ }"
    fi
  fi
}

ct()
{
    dir=`mktemp -d -t nephtmp.XXXXXXX`
    export NEPH_TEMP_DIR="$dir"
    cd "$dir"
    touch .nephtemp
}

clt()
{
  for x in /tmp/nephtmp.*; do
    [ -d "$x" ] || continue
    if [ -z "$(fuser "$x")" ]; then
      echo ":: Removing $x"
      rm -r $x
    else
      echo ":: Skipping $x (in use)"
    fi
  done
  [ ! -z "$NEPH_TEMP_DIR" ] && [ ! -d "$NEPH_TEMP_DIR" ] && unset NEPH_TEMP_DIR
}

rt()
{
    if [ ! -z "$NEPH_TEMP_DIR" ] && [ -d "$NEPH_TEMP_DIR" ]; then
        cd "$NEPH_TEMP_DIR"
    else
        unset NEPH_TEMP_DIR
        echo ":: No temp dir in context"
    fi
}

ipof() { host "$@" | awk '{print $(NF)}'; }

nb() { nmblookup "$@" | tail -n+2 | head -n1 | grep -Eo "^[^ ]+"; }
service() {
    if which systemctl &>/dev/null; then
        sudo systemctl --system daemon-reload
        sudo systemctl $2 $1
    elif [ -d /etc/rc.d ]; then
        sudo /etc/rc.d/$1 $2;
    else
        echo >&2 "!! Don't know how to modify services on this system"
        return 1
    fi
}

say()
{
    espeak --stdout "$*" | paplay
}

videolength()
{
    ffmpeg -i "$*" 2>&1 | grep Duration | cut -d ' ' -f 4 | sed s/,//
}

stopkde()
{
    kdeinit4_shutdown
    killall kded4 klauncher kdeinit4 knotify4
}

van()
{
    yelp "man:$@" &>/dev/null &
}

_quote() {
  echo \'${1//\'/\'\\\'\'}\'
}

x() {
  [ ! -z "$NEPH_CGROUP" ] && echo >&2 ":: WARNING: In cgroup"
  ("$@" >/dev/null 2>/dev/null &)
}
pic() { x gwenview "$@"; }
avant() { pkill avant-window; x avant-window-navigator; }

connectx()
{
  eval $(
    set -e
    . ~/bin/lib/util.sh
    get_x_session
    echo -n "export " && sh_var DISPLAY
    echo -n "export " && sh_var XAUTHORITY
  )
}

tf2()
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
    if [ ! -z "$(pidof hl2_linux)" ]; then
        echo ":: TF2 already running"
    else
        [ ! -z "$password" ] && cmdpassword="+password ${password}"
        [ ! -z "$host" ] && cmd="+connect ${host} $cmdpassword"
        c="steam -applaunch 440 $cmd"
        echo "Running command $c"
        eval "$c"
    fi
}

boost()
{
    pid="$(pgrep "$1")"
    for x in $(ls /proc/"$pid"/task); do
        # echo taskset -pc 0-3 $x
        taskset -pc 0-3 $x
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

[ ! -f ~/neph/priv/bashrc ] || . ~/neph/priv/bashrc

...() { echo ":-/"; }
