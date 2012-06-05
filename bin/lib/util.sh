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
