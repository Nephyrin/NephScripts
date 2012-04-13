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
