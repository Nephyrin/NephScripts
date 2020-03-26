#!/bin/bash

# WARNING: Shared aliases between bash/zsh, so needs to be compatible.

x() {
  [ ! -z "$NEPH_CGROUP" ] && echo >&2 ":: WARNING: In cgroup"
  ("$@" >/dev/null 2>/dev/null &)
}

pic() { x gwenview "$@"; }

rv() { cmd rsync -avy --progress "$@"; }
rvp() { cmd rsync -avy --partial --inplace --progress "$@"; }

ct()
{
    dir=`mktemp -d -t nephtmp.XXXXXXX`
    export NEPH_TEMP_DIR="$dir"
    cd "$dir"
    touch .nephtemp
}

clt()
{
  for x in /tmp/nephtmp.*; do
    [ -d "$x" ] || continue
    if [ -z "$(fuser "$x")" ]; then
      echo ":: Removing $x"
      rm -r $x
    else
      echo ":: Skipping $x (in use)"
    fi
  done
  [ ! -z "$NEPH_TEMP_DIR" ] && [ ! -d "$NEPH_TEMP_DIR" ] && unset NEPH_TEMP_DIR
}

rt()
{
    if [ ! -z "$NEPH_TEMP_DIR" ] && [ -d "$NEPH_TEMP_DIR" ]; then
        cd "$NEPH_TEMP_DIR"
    else
        unset NEPH_TEMP_DIR
        echo ":: No temp dir in context"
    fi
}

service() {
    if which systemctl &>/dev/null; then
        sudo systemctl --system daemon-reload
        sudo systemctl $2 $1
    elif [ -d /etc/rc.d ]; then
        sudo /etc/rc.d/$1 $2;
    else
        echo >&2 "!! Don't know how to modify services on this system"
        return 1
    fi
}

lx()  { ls++ --potsf     "$@"; }
lxx() { ls++ --potsf -tr "$@"; }
