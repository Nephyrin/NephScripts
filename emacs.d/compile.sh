#!/bin/bash

set -e

source ~/bin/lib/util.sh

cd "$(dirname "$0")"

cmd mkdir -pv "$PWD/neph-autoloads/"

estat Compiling Magit
( cmd cd magit && cmd make )

estat Compiling CEDET
( cmd cd cedet-git && cmd make )

estat Compiling Helm
( cmd cd helm && cmd make)

estat Compiling ECB
( cmd cd ecb && cmd make )

estat Making flyspell autoloads
cmd emacs -q --batch --eval "(update-file-autoloads \"$PWD/flyspell-lazy/flyspell-lazy.el\" t \"$PWD/neph-autoloads/neph-flyspell-lazy-autoload.el\")"

estat Compiling remaining modules in directory
cmd emacs --batch --eval "(load-file \"~/.emacs\")" --eval "(byte-recompile-directory \"$PWD\" 0)"

estat Done
