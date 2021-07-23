#!/bin/bash

# WARNING: Shared aliases between bash/zsh, so needs to be compatible.

# Throws every tunable at the job to beg it to compete for no resources.
#
# systemd-run is used to put it in a scope (and thus cgroup) with the cpu/io weight set to minimum.  The cgroup settings
# only have an effect if you have cgroup control delegated to your user slice in systemd and are using cgroups v2,
# however.
#
# nice is an older/simpler way to set CPU weight, which I believe is just factored in to the cgroup's weight in the
# CFS/cgroups world.  Including it is a nice fallback if cgroups aren't setup per above.  It also causes some system
# monitors to highlight the task as low priority, like htop, that don't currently know about cgroups.
#
# Finally, we set the scheduling priority of the task to 'idle'.  This kind of makes its weight a moot point.
#
# If kernel compiles still caused dropped frames running under this, I'm starting a crusade.
lowprio() {
  cmd systemd-run --user --scope         \
                  --property=CPUWeight=1 \
                  --property=IOWeight=1  \
                  nice -n20 chrt -i 0 "$@"
}

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

# Include local aliases
[[ ! -f $HOME/.aliases.local.sh ]] || source "$HOME"/.aliases.local.sh
