#!/bin/bash

set -e

source ~/bin/lib/util.sh

cd "$(dirname "$0")"

cmd mkdir -pv "$PWD/neph-autoloads/"

# Seomthing in here complains if this doesn't exist yet
cmd touch ede-projects.el

estat Compiling Async
[[ ! -f emacs-async/Makefile ]] || ( cmd cd emacs-async && cmd make )

estat Compiling Magit
[[ ! -f magit/Makefile ]] || ( cmd cd magit && cmd make GHUB_DIR=$PWD/../magit-ghub )

estat Compiling CEDET
[[ ! -f cedet-git/Makefile ]] || ( cmd cd cedet-git && cmd make )

estat Compiling Helm
[[ ! -f helm/Makefile ]] || ( cmd cd helm && cmd make LOADPATH="-L . -L ../emacs-async/" )

estat Compiling ECB
[[ ! -f ecb/Makefile ]] || ( cmd cd ecb && cmd make )

estat Compiling Evil
[[ ! -f evil/Makefile ]] || ( cmd cd evil && cmd make )

mkdir -pv "$PWD/neph-autoloads/"

autoload_onefile() {
  local name="$1"
  estat Making "$name" autoloads
  cmd emacs -q --batch --eval "(update-file-autoloads \"$PWD/$name/$name.el\" t \"$PWD/neph-autoloads/neph-$name-autoload.el\")"
}

autoload_dir() {
  local name="$1"
  local dir="$2"
  [[ -n $dir ]] || dir=$name
  cmd emacs -q --batch --eval "(let ((generated-autoload-file \"$PWD/neph-autoloads/neph-$name-autoload.el\"))  \
                                  (update-directory-autoloads \"$PWD/$dir\"))"

}

autoload_dir     evil
autoload_onefile flyspell-lazy
autoload_onefile projectile
autoload_onefile helm-projectile
autoload_dir     irony   irony-mode

estat Compiling remaining modules in directory
cmd emacs --batch --eval "(load-file \"~/.emacs\")" --eval "(byte-recompile-directory \"$PWD\" 0)"

estat Done
