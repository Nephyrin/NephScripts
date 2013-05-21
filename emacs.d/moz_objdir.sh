#!/bin/bash

set -e

mozinfo="$HOME/moz/neph.mozinfo"
mozdir="$(dirname "$mozinfo")"
file="$(readlink -f "$1")"
objdir="$2"

tree="${file#$mozdir/}"
[ "$tree" != "$file" ] || exit 1
tree="${tree%%/*}"

if [ -z "$objdir" ]; then
  eval mozinfo=($(cat "$mozinfo"))
  i=-2
  while [ -z "$objdir" ] && [ $(( i += 2 )) -lt ${#mozinfo[@]} ]; do
    [ "$tree" != "${mozinfo[$i]}" ] || objdir="${mozinfo[$(( i + 1 ))]}"
  done
fi

[ ! -z "$objdir" ] || exit 1

objdir="$mozdir/$objdir"
target="$objdir/${file#$mozdir/$tree/}"

while true; do
  if [ -z "$makedir" ]; then
    makedir="$(dirname "$target")"
  else
    makedir="$(readlink -f "$makedir/..")"
  fi
  [ "${makedir#$objdir}" != "$makedir" ] || exit 1
  [ ! -f "$makedir"/Makefile ] || break
done

cxxflags="$(sed -r 's/COMPILE_CXXFLAGS\s+=\s(.*)/\1/g;tx;d;:x' \
            < <(make -C "$makedir" showbuild))"

function relative() {
  if [ "${1#/}" != "$1" ]; then
    echo "$1"
  else
    echo "$makedir/$1"
  fi
}

function filterprint() {
  while [ $# -gt 0 ]; do
    arg="$1"
    shift
    if [ "$arg" = "-include" ]; then
      echo -include
      arg="$(relative "$1")"
      shift
    elif [ "${arg#-I}" != "$arg" ]; then
      arg="-I$(relative "${arg#-I}")"
    fi
    echo $arg
  done
}

echo $(filterprint ${cxxflags[@]})
