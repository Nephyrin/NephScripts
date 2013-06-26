#!/bin/bash

rightPad() {
  padding=$1
  shift
  str="$*"
  pad=$(( $padding - ${#str} ))
  printf "%s%${pad}s" "$str" ""
}

formatBytes() {
  bytes="$1"
  padding=$2
  [ ! -z "$padding" ] || padding=0

  num=$(( $1 + 0 ))
  div=1
  units=""
  if [ $num -gt $(( 1024 ** 3 )) ]; then
    div=$(( 1024 ** 3 ))
    units="GiB"
  elif [ $num -gt $(( 1024 * 1024 )) ]; then
    div=$(( 1024 * 1024 ))
    units="MiB"
  elif [ $num -gt $(( 1024 )) ]; then
    div=1024
    units="KiB"
  fi

  if [ -z "$units" ]; then
    ret="$num"B
  else
    ret=$(( ( num * 100 ) / div ))
    ind=$(( ${#ret} - 2 ))
    ret="${ret:0:$ind}.${ret:$ind}${units}"
  fi

  echo -n "$ret"
  pad=$(( $padding - ${#ret} ))
  if [ $pad -gt 0 ]; then
    printf "%${pad}s" " "
  fi
  echo
}

# Tries to connect to keychain, invalidates SSH_AGENT vars if it cannot.
try_keychain() {
  keyfile="$HOME/.keychain/$HOSTNAME-sh"
  [ ! -f "$keyfile" ] || source "$keyfile"
  if [ -z "$SSH_AUTH_SOCK" ] || \
     [ -z "$SSH_AGENT_PID" ] || \
     [ ! -e "$SSH_AUTH_SOCK" ] || \
     [ -z "$(ps -p "$SSH_AGENT_PID" -o comm=)" ]; then
    unset SSH_AUTH_SOCK
    unset SSH_AGENT_PID
    return 1
  fi
  return 0
}

sh_c()
{
  [ ! -z "$NEPH_COLOR_TERM" ] || return
  local c="$1"
  local b="$2"
  [ ! -z "$c" ] || c=0
  [ ! -z "$b" ] || [ $c -eq 0 ] || b=0
  [ -z "$b" ] || b="$b;"
  echo -n -e "\e[$b${c}m"
}

estat() { echo >&2 "$(sh_c 32 1)::$(sh_c) $*"; }
ewarn() { echo >&2 "$(sh_c 33 1);;$(sh_c) $*"; }
eerr() { echo >&2 "$(sh_c 31 1)!!$(sh_c) $*"; }

# Tries to find the running session for this user and steals its
# DISPLAY/XAUTHORITY env
get_x_session()
{
  pid=$(pgrep -o -u $USER gnome-session || true)
  [ -z "$pid" ] && pid=$(pgrep -o -u $USER xfce4-session || true)
  [ -z "$pid" ] && pid=$(pgrep -o -u $USER kded4 || true)
  if [ ! -z "$pid" ]; then
    echo >&2 ":: Stealing env from $pid"
    export $(cat /proc/$pid/environ | grep -z XAUTHORITY)
    export $(cat /proc/$pid/environ | grep -z DISPLAY)
  else
      export XAUTHORITY=$HOME/.Xauthority
  fi
  [ ! -z "$DISPLAY" ] || export DISPLAY=:0
  [ ! -z "$XAUTHORITY" ] || export XAUTHORITY=$HOME/.Xauthority
}

# Prints eval-able expression to set given variable, e.g.:
# sh_var DISPLAY -> "DISPLAY=':0'"
sh_var()
{
  eval local ret="\$$1"
  echo $1=\'${ret//\'/\'\\\'\'}\';
}
