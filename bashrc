# -*- mode: sh; sh-basic-offset: 2; sh-indentation: 2; -*-

# Uses util.sh from nephscripts
if [ -r ~/bin/lib/util.sh ]; then
  source ~/bin/lib/util.sh
else
  # Check same dir as the source bashrc.
  # This makes things like: `ln -sv ~/nephscripts/bashrc ~/.bashrc` work without
  # using the whole nephscripts bin/
  _neph_bashrc_dir="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
  if [[ -n "$_bashrc_dir" && -r "$_bashrc_dir/bin/lib/util.sh" ]]; then
    source "$_bashrc_dir/bin/lib/util.sh"
  else
    echo >&2 "!! No util.sh found, this (nephscripts) bashrc uses it. Expect blarg."
  fi
  unset _neph_bashrc_dir
fi

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

NEPH_DEFAULT_CGROUP=/sys/fs/cgroup/cpu

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

export P4CONFIG=P4CONFIG

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
export MOZHG=mozilla-hg
[ ! -f ~/bin/lib/moz.sh ] || source ~/bin/lib/moz.sh

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
          if [ -n "$SSH_CLIENT" ]; then
            PS1='\[\e[32m\]\u\[\e[90m\]@\[\e[33m\]\h\[\e[01;34m\] \w \$\[\e[00m\] '
          else
            PS1='\[\e[32m\]\u\[\e[01;34m\] \w \$\[\e[00m\] '
          fi
        fi

        ls() { ( unset ls && ls --color=auto "$@" ) }
        grep() { ( unset grep && grep --colour=auto "$@" ) }
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

pn() { promptnote "$@"; }

if ( ! which pidof && which pgrep ) &>/dev/null; then
  pidof() { cmd pgrep -x "$@"; }
fi

please() { cmd eval sudo "$(fc -nl -1)"; }

hgreset() {
  [[ $# -eq 0 ]] || err "Unexpected arguments: $*"
  hg strip 'roots(outgoing())' && hg up -C && hg purge --all && hg status
}
rv() { rsync -avy --partial --progress "$@"; }

iswine() { for x in wine exe; do pf "$x"; done; }

# Git's nicer diff can be used with --no-index to diff random things
giff() { git diff --no-index --color=auto "$@"; }

xg() { x gedit "$@"; }
xk() { x kate "$@"; }
y() { yaourt "$@"; }
yclean() { yaourt -Rs $(y -Qdtq) "$@"; }
p() { sudo pacman "$@"; }

dir() { x dolphin "$@"; }

rebash() { source ~/.bashrc "$@"; }

lx()  { ls++ --potsf     "$@"; }
lxx() { ls++ --potsf -tr "$@"; }

agc() { ag --cpp "$@"; }

# Like cd, but do a { readlink -f } and cd to the ultimate target
# With no argument, defaults to $PWD (unlike cd) to simply canonicalize current dir
recd() { cd "$(readlink -f "${*-$(pwd)}")"; }

qvnc() { cmd vncviewer -QualityLevel 9 -NoJPEG -CompressLevel 6 "$@"; }

#
# Misc utility functions
#

# Shorthand rdp
rdp()
{
  rdesktop -x l -g 1920x1200 -a 32 "$@"
}

# Change to git toplevel directory or error
gt() {
  local top
  # Will show useful error if not a git directory and return empty
  top="$(git rev-parse --show-toplevel)"
  [[ -z $top ]] || cmd cd "$top"
}

# 99% of my find invocations
fn() {
  local name="$1"
  shift
  find . -iname "*$name*" "$@"
}

pathadd() {
  if [[ $# -le 0 ]]; then
    ewarn "Usage: pathadd <path> [<path...>]"
    return 1
  fi

  local item
  for item in "$@"; do
    _list_push PATH "$(readlink -f "$item")"
  done
}

# This is necessary so that NEPH_CGROUP_PS1 and MOZ_PS1 are expanded now, such
# that \[\] escapes can be recognized, rather than literally in the prompt,
# wherien they would not be after expansion.
BASE_PS1="$PS1"
_reprompt() {
  if [[ $(type -t _priv_reprompt) = function ]]; then
    _priv_reprompt
  else
    unset PRIV_PS1
  fi
  PS1="$NEPH_PROMPTNOTE_PS1$NEPH_CGROUP_PS1$PRIV_PS1$MOZ_PS1$NEPH_GIT_PS1$BASE_PS1"
}
_reprompt

# The prompt command could be just a big eval statement and avoid needing this,
# but we bake in things like NEPH_PROMPTNOTE and hostname that either don't
# change or can call _reprompt_command when they do
HISTORY_PROMPT_COMMAND="${HISTORY_PROMPT_COMMAND:=history -a}"

_reprompt_command() {
  PROMPT_COMMAND="$HISTORY_PROMPT_COMMAND"

  # Window title
  if [ -z "$TMUX" ]; then # tmux.conf handles this
    NEPH_TERM_TITLE='${PWD/$HOME/\~}'
    if [[ -n $NEPH_PROMPTNOTE ]]; then
      NEPH_TERM_TITLE="[$NEPH_PROMPTNOTE] $NEPH_TERM_TITLE"
    fi
    if [[ $HOSTNAME != $(hostname) ]]; then
      NEPH_TERM_TITLE='${HOSTNAME%%.*}: '"$NEPH_TERM_TITLE"
    fi

    [[ -z $PROMPT_COMMAND ]] || PROMPT_COMMAND="$PROMPT_COMMAND;"
    case ${TERM} in
      xterm*|rxvt*|aterm|kterm|gnome*|interix)
        PROMPT_COMMAND="$PROMPT_COMMAND"'echo -ne "\e]0;'"$NEPH_TERM_TITLE"'\007"'
        ;;
      screen*)
        PROMPT_COMMAND="$PROMPT_COMMAND"'echo -ne "\e_'"$NEPH_TERM_TITLE"'\e\\"'
        ;;
    esac
  fi
}
_reprompt_command

# Add a note to the prompt
promptnote() {
  local note="$*"
  if [ -n "$note" ]; then
    NEPH_PROMPTNOTE="$note"
    NEPH_PROMPTNOTE_PS1="\[\e[37;2m\]~\[\e[32;2m\]$note\[\e[37;2m\]~\[\e[0m\] "
  else
    unset NEPH_PROMPTNOTE
    unset NEPH_PROMPTNOTE_PS1
  fi
  _reprompt
  _reprompt_command
}

# Stop writing history for this session
nohist()
{
  HISTORY_PROMPT_COMMAND=":"
  unset HISTFILE
  _reprompt_command

  local note="$NEPH_PROMPTNOTE"
  [[ -z $note ]] || note="$note, "
  promptnote "${note}no history"
}

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

rand32()
{
  local r
  let "r = $RANDOM << 17 | $RANDOM << 2 | ( $RANDOM & 0x3 )"
  echo $r
}

dcg()
{
  if [[ -z $NEPH_DEFAULT_CGROUP || -z $NEPH_CGROUP ]]; then
    eerr "Invalid cgroup"
    return 1
  fi

  echo $$ > "$NEPH_DEFAULT_CGROUP"/cgroup.procs
  cmd rmdir "$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"
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
  local prio="$1"
  local mem="$2"
  [[ -n $prio ]] || prio=1
  [[ -n $mem ]] || mem=256M
  [[ -z $NEPH_CGROUP ]] && cg
  group=$NEPH_CGROUP
  group="$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"
  [[ ! -d $group ]] && echo ":: '$group' is not a directory" && return

  /bin/echo "$prio" > "$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"/cpu.shares

  local blkioweight="$NEPH_DEFAULT_CGROUP/$NEPH_CGROUP"/blkio.weight
  local memlimit="$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"/memory.soft_limit_in_bytes

 [[ ! -e $blkioweight ]] || /bin/echo 10   > $blkioweight
 [[ ! -e $memlimit    ]] || /bin/echo "$mem" > $memlimit

  echo ":: Created low prio cgroup with $prio cpu shares and $mem soft memory limit"
}

cdcg()
{
  if [[ -z $NEPH_DEFAULT_CGROUP || -z $NEPH_CGROUP ]]; then
    echo >&2 "!! No cgroup"
    return 1
  fi
  cmd cd "$NEPH_DEFAULT_CGROUP"/"$NEPH_CGROUP"
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

  local cpus="$NEPH_DEFAULT_CGROUP"/"$name"/cpuset.cpus
  local mems="$NEPH_DEFAULT_CGROUP"/"$name"/cpuset.mems

  [[ ! -e $cpus ]] || cat "$NEPH_DEFAULT_CGROUP"/cpuset.cpus > $cpus
  [[ ! -e $mems ]] || cat "$NEPH_DEFAULT_CGROUP"/cpuset.mems > $mems

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
