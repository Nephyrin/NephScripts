#!/bin/bash

set -e

source ~/bin/lib/util.sh

cd "$(dirname "$0")"

estat Compiling CEDET
( cmd cd cedet-git && cmd make )

estat Compiling Helm
( cmd cd helm && cmd make)

estat Compiling remaining modules in directory
cmd emacs --batch --eval "(byte-recompile-directory \"$PWD\" 0)"

estat Done
