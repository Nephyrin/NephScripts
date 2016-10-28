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

estat Compiling Evil
( cmd cd evil && cmd make )

estat Making Evil autoloads
cmd emacs -q --batch --eval "(let ((generated-autoload-file \"$PWD/neph-autoloads/neph-evil-autoload.el\"))  \
                                  (update-directory-autoloads \"$PWD/evil\"))"

estat Making flyspell autoloads
cmd emacs -q --batch --eval "(update-file-autoloads \"$PWD/flyspell-lazy/flyspell-lazy.el\" t \"$PWD/neph-autoloads/neph-flyspell-lazy-autoload.el\")"

estat Making projectile autoloads
cmd emacs -q --batch --eval "(update-file-autoloads \"$PWD/projectile/projectile.el\" t \"$PWD/neph-autoloads/neph-projectile-autoload.el\")"

estat Making helm-projectile autoloads
cmd emacs -q --batch --eval "(update-file-autoloads \"$PWD/helm-projectile/helm-projectile.el\" t \"$PWD/neph-autoloads/neph-helm-projectile-autoload.el\")"

estat Compiling remaining modules in directory
cmd emacs --batch --eval "(load-file \"~/.emacs\")" --eval "(byte-recompile-directory \"$PWD\" 0)"

estat Done
