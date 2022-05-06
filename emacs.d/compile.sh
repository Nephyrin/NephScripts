#!/bin/bash

set -euo pipefail

source ~/bin/lib/util.sh

cd "$(dirname "$0")"

cmd mkdir -pv "$PWD/neph-autoloads/"

# Seomthing in here complains if this doesn't exist yet
cmd touch ede-projects.el

on_err() { ret=$?; eerr "Failed, see above"; exit $ret; }
trap on_err ERR

compile() {
  [[ ! -d .git ]] || git clean -xfdn .
  if [[ -f "$1"/Makefile ]]; then
    estat "Compiling $1"
    ( cmd cd "$1" && cmd make -j$(nproc) "${@:2}" )
  else
    ewarn "No makefile for $1, skipping"
  fi
}

compile dash
compile git-modes
compile with-editor
compile emacs-async
compile helm LOADPATH="-L . -L ../emacs-async/"
compile ecb
compile evil
compile emacs-gdb

mkdir -pv "$PWD/neph-autoloads/"

autoload_onefile() {
  local name="$1"
  estat Making "$name" autoloads
  cmd emacs -q --batch --eval "(update-file-autoloads \"$PWD/$name/$name.el\" t \"$PWD/neph-autoloads/neph-$name-autoload.el\")"
}

autoload_dir() {
  local name="$1"
  local dir="${2-}"
  [[ -n $dir ]] || dir=$name
  cmd emacs -q --batch --eval "(progn (setq generated-autoload-file \"$PWD/neph-autoloads/neph-$name-autoload.el\")  \
                                  (update-directory-autoloads \"$PWD/$dir\"))"

}

autoload_dir     evil
autoload_onefile flyspell-lazy
autoload_onefile projectile
autoload_onefile helm-projectile
autoload_dir     irony        irony-mode
autoload_dir     company      company-mode
autoload_dir     ycmd         emacs-ycmd
autoload_dir     weirdnox-gdb emacs-gdb
autoload_dir     yaml-mode    yaml-mode

estat Compiling remaining modules in directory
cmd emacs --batch --eval "(load-file \"~/.emacs\")" --eval "(byte-recompile-directory \"$PWD\" 0)"

estat Done
