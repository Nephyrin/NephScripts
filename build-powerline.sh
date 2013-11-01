#!/bin/bash

set -e

cd "$(dirname "$0")"
. bin/lib/util.sh

if [ -d "./powerline" ]; then
  die "./powerline already exists, delete it first"
fi

pyver="$(( python -V 2>&1 || true ) \
         | sed -r 's/^Python ([0-9]+\.[0-9]+).*$/\1/;t;d')"
[ ! -z "$pyver" ] || die "Couldn't find current python version"
estat "Python version is $pyver"
export PYTHONPATH="$PWD/powerline/lib/python${pyver}/site-packages/"
prefix="$PWD/powerline/"

cmd mkdir -pv "$PYTHONPATH"
cmd cd powerline.git
cmd python ./setup.py install --prefix="$prefix"
cmd cd ../powerline
cmd ln -sv lib/python${pyver}/site-packages/Powerline*egg/powerline/bindings
cmd cd lib
cmd ln -sv "python${pyver}" python-latest
estat "Powerline installed into ${prefix}, see bashrc for PATH/PYTHONPATH"
