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

# Run a command detached from the shell, with no connection to the tty or return code, etc..
# Can return failure if the given arguments don't map to a runnable command, and will still print the relevant error.
#
# Commands that parse to a valid file/function/etc, but cannot be launched -- e.g. due to permissions errors -- will
# still silently succeed.  This is a limitation of bash/zsh as far as I can tell: that class of failure is simply the
# same as the application failing early.
#
# Ex: A `chmod ugo=x` shell script will be parsed as valid by `command -v`, but hit a permissions error prior to bash
# creating a child process to run it.  From the shell's perspective, that command simply failed quickly, and any
# complaining the shell itself writes to stderr is part of the command's output.
#
# Update: The s() alias below is a better way to do this in a systemd system.
x() {
  # Warn if you're accidentally launching something in a cgroup (since that is not detached) using the neph cgroup
  # functions.
  [[ -z $NEPH_CGROUP ]] || ewarn "WARNING: In cgroup"

  # If `command -v` doesn't think this parses as something runnable, just run it bare so the shell-level
  # error/error-code occurs.  This means that `x some_typo --args` doesn't silently succeed, but errors exactly as
  # `some_typo --args` would.
  if command -v "${1-}" &>/dev/null; then
    ( "$@" &>/dev/null & disown || true ) # disown could fail if the job didn't start for a reason other than it not
                                          # being a valid command.  I'm not sure if there's a way in bash/zsh to say
                                          # "show me all the errors up to opening/exec'ing the command"
  else
    "$@"
  fi
}

# spawns a command with systemd-run, like x() but good for spawning something in the desktop session with proper
# cgrouping and such like top-level apps enjoy.
s()
{
  # --user - as this user, not system-level command
  # --same-dir - keep this working directory
  # --collect - don't leave failed units around for inspection
  cmd systemd-run --user --same-dir --collect -- "$@"
}

# Re-execute or switch shell (to load new configs or whatever clearly)
#
# if $SHELL is unset or missing you could `exec /proc/self/exe`, but that's weird enough that an alias shouldn't just do
# it (consider if $SHELL is a wrapper)
reload() { cmd exec -- "$SHELL"; }
rezsh() { cmd exec zsh; }
rebash() { cmd exec bash; }

pic() { s gwenview "$@"; }

rv() { cmd rsync -avy --progress "$@"; }
rvp() { cmd rsync -avy --partial --inplace --progress "$@"; }

iswine()
{
  local winepids=($(egrep -lzEi '^WINELOADERNOEXEC=' /proc/*/environ 2>/dev/null \
                      | sed -r 's/^\/proc\/([0-9]+)\/environ$/\1/'))
  [[ ${#winepids[@]} -gt 0 ]] && cmd ps -fp "${winepids[@]}" || ewarn No wine processes found
}

ct()
{
    dir=$(mktemp -d -t nephtmp.XXXXXXX)
    export NEPH_TEMP_DIR="$dir"
    cd "$dir"
}

clt()
{
  # (This matches how mktemp picks)
  local tmpdir=${TMPDIR:-/tmp}
  for x in "$tmpdir"/nephtmp.*; do
    [[ -d $x ]] || continue
    local pids
    pids=($(lsof -Fp +D "$x" | tr -d p))
    if [[ -z $pids ]]; then
      estat "Removing $x"
      cmd rm -rf --one-file-system -- "$x"
      # If this is our cached directory nuke it
      [[ $x != ${NEPH_TEMP_DIR-} ]] || unset NEPH_TEMP_DIR
    else
      estat "Skipping $x, in use:"
      ps -p "${pids[@]}" | einfo_pipe
    fi
  done
}

rt()
{
  if [[ -n $NEPH_TEMP_DIR && -d $NEPH_TEMP_DIR ]]; then
    cd -- "$NEPH_TEMP_DIR"
  else
    unset NEPH_TEMP_DIR
    estat "No temp dir in context"
  fi
}

service() {
  if which systemctl &>/dev/null; then
    cmd sudo systemctl -- "$2" "$1"
  elif [[ -d /etc/rc.d ]]; then
    cmd sudo /etc/rc.d/"$1" "$2";
  else
    eerr "Don't know how to modify services on this system"
    return 1
  fi
}

lx()  { ls++ --potsf     "$@"; }
lxx() { ls++ --potsf -tr "$@"; }

v() { cmd youtube-dl "$@"; }

# Include local aliases
[[ ! -f $HOME/.aliases.local.sh ]] || source "$HOME"/.aliases.local.sh
