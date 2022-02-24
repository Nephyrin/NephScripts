# -*- mode: sh; sh-basic-offset: 2; sh-indentation: 2; -*-

if [[ $TERM_PROGRAM = Apple_Terminal ]]; then
    PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
    if [[ -n $MANPATH ]]; then
      export MANPATH=":$MANPATH"
    fi
    MANPATH="/usr/local/opt/coreutils/libexec/gnuman$MANPATH"
fi

NEPH=~/neph
NPRIV=~/neph/priv


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
  eval s=\(\$"$1"\)
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

NEPH_CGROUP_ROOT=/sys/fs/cgroup
NEPH_DEFAULT_CGROUP="$NEPH_CGROUP_ROOT"/cpu

[ -z "$XAUTHORITY" ] && export XAUTHORITY=$HOME/.Xauthority

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
export MOZHG=mozilla-hg
[ ! -f ~/bin/lib/moz.sh ] || source ~/bin/lib/moz.sh

#
# Bash-specific interactive tweaks
#
if [[ -n $BASH && -n $BASH_VERSION && $- == *i* ]] ; then
    # Shared interactive shell startup
    [[ ! -f "$NEPH"/interactive-shell-init.sh ]] || source "$NEPH"/interactive-shell-init.sh

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
            PS1='\[\e[32m\]\u\[\e[90m\]@\[\e[33m\]\h\[\e[01;34m\] \w \[\e[0;31m\]❯\[\e[33m\]❯\[\e[32m\]❯\[\e[00m\] '
          else
            PS1='\[\e[32m\]\u\[\e[01;34m\] \w \[\e[01;31m\]❯\[\e[01;33m\]❯\[\e[01;32m\]❯\[\e[00m\] '
          fi
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

rcl() { reset; clear; }
nts() { gawk '{ print strftime("[%Y-%m-%d %H:%M:%S]"), $0 }'; }

pn() { promptnote "$@"; }

if ( ! which pidof && which pgrep ) &>/dev/null; then
  pidof() { cmd pgrep -x "$@"; }
fi

please() { cmd eval sudo "$(fc -nl -1)"; }

hgreset() {
  [[ $# -eq 0 ]] || err "Unexpected arguments: $*"
  hg strip 'roots(outgoing())' && hg up -C && hg purge --all && hg status
}

iswine() { for x in wine exe; do pf "$x"; done; }

xg() { x gedit "$@"; }
xk() { x kate "$@"; }
p() { sudo pacman "$@"; }

! alias dir &>/dev/null || unalias dir
dir() { x dolphin "$@"; }

rebash() { source ~/.bashrc "$@"; }

ag() { $(which ag) --noaffinity "$@"; }
agc() { ag --cpp "$@"; }

# Shorthand readlink -f, with no args will resolve PWD
rl() { local args=("$@"); [[ ${#args[@]} -gt 0 ]] || args=(.); readlink -f -- "${args[@]}"; }

# Like cd, but do a { readlink -f } and cd to the ultimate target
# With no argument, defaults to $PWD (unlike cd) to simply canonicalize current dir
recd() { cd "$(readlink -f "${*-$(pwd)}")"; }

qvnc() { cmd vncviewer -QualityLevel 9 -NoJPEG -CompressLevel 6 "$@"; }

# Until systemd has a not-shit interface to this. Cgmanager sux.
cghax()
{
  cmd sudo chown -v root:"$USER" /sys/fs/cgroup/{cpu,blkio,memory}/{.,cgroup.procs,tasks}
  cmd sudo chmod -v g+w /sys/fs/cgroup/{cpu,blkio,memory}/{.,cgroup.procs,tasks}
}

steamrt() {
  cmd ~/.steam/steam/ubuntu12_32/steam-runtime/run.sh "$@"
}

# Shorthand for invoking date --date="A" [B]
# If passing additional arguments, show the results of date --date="A" with and without them
ddate() {
  if [[ $# -eq 0 ]]; then
    cmd date
  else
    [[ $# -le 1 ]] || cmd date --date="$1"
    cmd date --date="$1" "${@:2}"
  fi
}

# Print a timestamp or timestamp for a date
tt() {
  local date
  if [[ $# -eq 0 ]]; then
    date="now"
  elif [[ $# -eq 1 && $1 =~ ^[0-9]+$ ]]; then
    date="@$1"
  else
    # Treat as date
    date="$*"
  fi
  einfo "Parsed"
  date --date="$date" || return
  einfo "Timestamp"
  date --date="$date" +%s
}

#
# fzf stuff
#
check_fzf()
{
  if ! type fzf &>/dev/null; then
    eerr fzf not available
    return 1
  fi
}

fzcd() { _fzcd_int "-type d" "$@"; }
fzfd() { _fzcd_int "" "$@"; }

fzvd() {
  if [[ -z $VALVE_ROOT ]]; then
    err No \$VALVE_ROOT
    return 1
  fi
  fzfd "$VALVE_ROOT"
}

# Run fzf, store result into $fzret
fzvar() {
  if ! check_fzf; then return 1; fi
  [[ -z ${fz+x} ]] || einfo "Previous \$fz -> $(printf %q "$fz")"
  fz="$(fzf "$@")"
  emsg "Set new \$fz  -> $(printf %q "$fz")"
}

_fzcd_int()
{
  if ! check_fzf; then return 1; fi

  local type="$1"
  local in=("${@:2}")
  if [[ ${#in[@]} -eq 0 || -z ${in[*]} ]]; then
    local dir="$(find . $type | sed -r 's/^.\/(.+)$/\1/;t;/^.$/ d' | fzf)"
  else
    local dir="$(find "${in[@]}" $type | fzf)"
  fi
  if [[ $? -ne 0 || -z $dir ]]; then
    ewarn "No directory selected"
    return 1
  fi
  [[ -d $dir || ! -d $(dirname "$dir") ]] || dir="$(dirname "$dir")"
  cmd cd "$dir"
}

#
# Misc utility functions
#

# Shorthand rdp
rdp()
{
  cmd xfreerdp /dynamic-resolution /scale-desktop:140 /scale:140 /scale-device:140 /clipboard /w:1920 /h:1200 /v:"$1" "${@:2}"
}

# Change to git toplevel directory or error
gt() {
  local top
  # Will show useful error if not a git directory and return empty
  top="$(git rev-parse --show-toplevel)"
  [[ -z $top ]] || cmd cd "$top"
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

# Print the word corresponding to type for supported shells
_get_type() {
  local type
  local arg="$1"
  if n_is_bash; then
    type -t "$arg"
  elif n_is_zsh; then
    echo ${$(whence -w $arg)[2]}
  fi
}

# Is given type a function
_is_func() {
  [[ $# -ge 1 && $(_get_type "$*") = function ]]
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
  PS1="$_NEPH_PN_PS1$NEPH_CGROUP_PS1$PRIV_PS1$MOZ_PS1$NEPH_GIT_PS1$BASE_PS1"
}
_reprompt

# The prompt command could be just a big eval statement and avoid needing this,
# but we bake in things like _NEPH_PN and hostname that either don't
# change or can call _reprompt_command when they do
HISTORY_PROMPT_COMMAND="${HISTORY_PROMPT_COMMAND:=history -a}"

_reprompt_command() {
  PROMPT_COMMAND="$HISTORY_PROMPT_COMMAND"

  # Window title
  if [ -z "$TMUX" ]; then # tmux.conf handles this
    NEPH_TERM_TITLE='${PWD/$HOME/\~}'
    if [[ -n $_NEPH_PN ]]; then
      NEPH_TERM_TITLE="[$_NEPH_PN] $NEPH_TERM_TITLE"
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
    _NEPH_PN="$note"
    _NEPH_PN_PS1="\[\e[37;2m\]~\[\e[32;2m\]$note\[\e[37;2m\]~\[\e[0m\] "
  else
    unset _NEPH_PN
    unset _NEPH_PN_PS1
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

  local note="$_NEPH_PN"
  [[ -z $note ]] || note="$note, "
  promptnote "${note}no history"
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

_setcgname()
{
  local name="$1"
  export NEPH_CGROUP=$name
  # 3 italic, 90 grey, 23 cancel italic yay
  NEPH_CGROUP_PS1='\['$(sh_c 3 90)'\]'$NEPH_CGROUP'\['$(sh_c 23)'\]'" "
  _reprompt
}

leavecg()
{
  if [[ -z $NEPH_DEFAULT_CGROUP || -z $NEPH_CGROUP ]]; then
    eerr "Invalid cgroup"
    return 1
  fi

  local dir
  for dir in "$NEPH_CGROUP_ROOT"/*; do
    if [[ ! -L $dir && -d $dir/$NEPH_CGROUP ]]; then
      einfo "Removing self from $dir/$NEPH_CGROUP"
      echo "$$" > "$dir/tasks" 2>/dev/null
    fi
  done

  unset NEPH_CGROUP
  unset NEPH_CGROUP_PS1
  _reprompt
}

dcg()
{
  local wascg="$NEPH_CGROUP"
  leavecg "$@" || return 1

  local dir
  for dir in "$NEPH_CGROUP_ROOT"/*; do
    [[ ! -L $dir && -d $dir/$wascg ]] || continue
    cmd rmdir "$dir/$wascg"
    if [[ -d $dir/$wascg ]]; then
      eerr "Failed to delete $dir/$wascg - has other processes:"
      cmd ps -p $(cat "$dir/$wascg/cgroup.procs")
    fi
  done
}

jcg()
{
  if [[ -z $NEPH_DEFAULT_CGROUP  ]]; then
    eerr "Invalid cgroup config"
    return 1
  fi

  local name="$1";
  if [[ $# -lt 1 || -z $name ]]; then
    eerr "Usage: jcg <name>";
    return 1;
  fi;

  local dir
  for dir in "$NEPH_CGROUP_ROOT"/*; do
    [[ ! -d $dir/$name ]] || ecmd echo \$\$ \> "$dir/$name/cgroup.procs"
  done

  _setcgname "$name"
}

lowprio()
{
  if [[ -n $NEPH_CGROUP ]]; then
    ewarn "Warning, leaving old cgroup: $NEPH_CGROUP"
    dcg
  fi
  # Create and move into new low priority cgroup
  lpcg lowprio-cg
  # Run command with minimal nice/ionice perms (to get pretty blue colors in htop, if we're being
  # honest)
  time ionice -c3 nice -n20 "$@"
  # Delete low prio group
  dcg
}

lpcg()
{
  local name="$1";
  local cpu="$2";
  local blkio="$3";
  if [[ $# -lt 1 || $# -gt 3 || -z $name ]]; then
    eerr "Usage: lpcg <name> [<cpu shares = 20> [<blkio weight = 10>]]";
    return 1;
  fi;
  [[ -n $cpu ]] || cpu=20;
  [[ -n $blkio ]] || blkio=10;
  cg "$name" cpu blkio || return 1;
  local cgcpu=/sys/fs/cgroup/cpu/"$name";
  local cgblkio=/sys/fs/cgroup/blkio/"$name";
  echo $cpu > "$cgcpu"/cpu.shares;
  echo $blkio > "$cgblkio"/blkio.weight;
  estat "Setup cgroup $name with cpu.shares $cpu and blkio.weight $blkio"
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
# Needs to be fixed for new CG commands
#pcg()
#{
#  local procgrep="$1"
#  local cgroup="$2"
#  [ -z "$procgrep" ] && echo ":: Need a taskname" && return
#  local tasks=$(pgrep "$procgrep")
#
#  [ -z "$cgroup" ] && cgroup=$(_mcg "$procgrep")
#  if [ ! -d "$NEPH_DEFAULT_CGROUP"/"$cgroup" ]; then
#    echo ":: cgroup $cgroup does not exist!"
#    return
#  fi
#
#  for task in $tasks; do
#      echo ":: Adding task $task to group '$cgroup'"
#      /bin/echo $task > "$NEPH_DEFAULT_CGROUP"/"$cgroup"/cgroup.procs
#  done
#}

# Make a control group with prefix
mcg()
{
  # Pretty version
  local name=$(_mcg "$@")
  local types=("${@:2}")
  if [[ -z $name ]]; then
    eerr "Failed to create cgroup: $*"
    return 1
  fi

  einfo "Created empty control groups $name / (${types[@]})"
}

_mcg()
{
  local name="$1"
  local types=("${@:2}")
  [[ $# -lt 2 || ${#types[@]} = 0 || -z $name ]] && eerr "Usage: _mcg <name> <types>" && return 1
  local i

  local type
  for type in "${types[@]}"; do
    local cgdir=
    if [[ -d $NEPH_CGROUP_ROOT/$type/$name ]]; then
      eerr "cgroup already exists: $type/$name"
      return 1
    fi
  done

  for type in "${types[@]}"; do
    if ! cmd mkdir "$NEPH_CGROUP_ROOT/$type/$name"; then
      eerr "Could not create cgroup $type/$name"
      return 1
    fi
  done

  echo $name
}

cg()
{
  local name="$1"
  local types=("${@:2}")
  [[ -n $name ]] || name="shell$$"
  [[ ${#types[@]} -ge 1 ]] || types=(cpu)
  local i

  mcg "$name" "${types[@]}" || return 1

  for type in "${types[@]}"; do
    echo $$ > "$NEPH_CGROUP_ROOT"/"$type"/"$name"/cgroup.procs
  done

  _setcgname $name
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
      eerr "cgroup $cgroup doesn't exist";
      return
    fi
    einfo "Tasks for $cgroup"
    local tasks="$(cat "$NEPH_DEFAULT_CGROUP"/"$cgroup"/tasks)"
    if [ -z "$tasks" ]; then
      echo " none"
    else
      ps -o pid= -o comm= -p "${tasks/$'\n'/ }"
    fi
  fi
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

ipof() { host "$@" | awk '{print $(NF)}'; }

nb() { nmblookup "$@" | tail -n+2 | head -n1 | grep -Eo "^[^ ]+"; }

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

[[ ! -f $NEPH/aliases.sh ]] || source "$NEPH"/aliases.sh
[[ ! -f $NPRIV/bashrc ]]    || source "$NPRIV"/bashrc
[[ ! -f ~/.bashrc.local ]]  || source ~/.bashrc.local
