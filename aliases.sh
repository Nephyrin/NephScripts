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
    ( "$@" &>/dev/null & )
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

# spawns a detached command with systemd-run, but *without* preserving working directory.
# This is just s() without `--same-dir`
#
# This is useful for spawning long-running things that don't care about working directory, without accidentally keeping
# a reference to random temp directories or external mounts etc related to that process.
S() {
  cmd systemd-run --user --collect -- "$@"
}

# Set window title and tmux tab name
# Note that PS1/PROMPT_COMMANDs often reset window titles
t() {
  # https://tldp.org/HOWTO/Xterm-Title-3.html
  # ESC]0;stringBEL -- Set icon name and window title to string
  # ESC]1;stringBEL -- Set icon name to string
  # ESC]2;stringBEL -- Set window title to string
  #
  # where ESC is the escape character (\033), and BEL is the bell character (\007).
  [[ -t 1 ]] && printf "\033]0;%s\007" "$*"
  # Opportunistically set tmux window name too.
  tmux renamew "$*" &>/dev/null || true
}

# zdir <archive> [dirname]
#
# Unarchive something into a directory named after the zip file (without extention, using `autounzip` utility from
# nephscripts) in the current directory and then cd into it.
#
# Optionally specify the output dir name.
zdir() {
  [[ -n ${1-} && $# -le 2 && ( -n ${2-} || $# -lt 2 ) ]] || { eerr 'Usage: zdir <archive> [dirname]'; return 1; }
  local archive=$1
  local outdir=${2-}
  archive=$(readlink -f "$archive")

  if [[ -z $outdir ]]; then
    outdir=$(basename -- "$archive")
    outdir=${outdir%.*}
  fi

  [[ -n $outdir ]] || { eerr "Couldn't determine output name for archive \"$archive\""; return 1; }

  einfo "Output dir $outdir"
  cmd autounzip -- "$archive" "$outdir"
  cmd cd -- "$outdir"
}

# marchive <archive> [<archive> ...]
#
# Moves the archive to $PWD and then runs autounzip on it.
#
# Very specific, for pulling things out of ~/Downloads and archiving them elsewhere mostly.
marchive() {
  local usage="Usage: marchive <archive> [<archive> ...]"
  [[ $# -ge 1 ]] || { eerr "$usage"; return 1; }

  local archive
  for archive in "$@"; do
    [[ -n $archive && -f $archive ]] || { eerr "$usage"; return 1; }
    local base
    base=$(basename -- "$archive")
    [[ -n $base && ! -e $base ]] || { eerr "Usage: Bad target to move archive to: \"$base\""; return 1; }
    cmd mv -v -- "$archive" "$base"
    cmd autounzip "$base"
  done
}

# Try to setup a fully clean environment, looking at user's session and etc.  Preserves $TERM if set, but that's it.
#
# Looks at systemd session variables, but isn't going to restore things like SSH specific or DE specific things that've
# been inherited.
#
# Arguments are a list of variables to preserve
ncleanenv() {
  # Roughly init_environ does in /bin/login.  Closest to being a standard for what fresh environments should look like.
  # Doesn't query PAM though.  I wish /bin/login had a no-auth mode that just re-evaluated the environment.
  local user && user=$(/bin/id -un)
  local uid && uid=$(/bin/id -u)
  local ent && ent=$(/bin/getent passwd "$user")
  local home && home=$(cut -d ':' -f 6 <<< "$ent")
  local shell && shell=$(cut -d ':' -f 7 <<< "$ent")
  local vars=()
  vars+=(USER="$user")
  vars+=(HOME="$home")
  vars+=(LOGNAME="$user")
  vars+=(SHELL="$shell")

  # Check what we have exported against the preserve list (and $TERM)
  local envvar var preserve
  while IFS= read -r -d $'\0' envvar; do
    var=${envvar%%=*}
    for preserve in TERM "$@"; do
      if [[ $var = "$preserve" ]]; then
        vars+=("$envvar")
      fi
    done
  done < <(/bin/env -0)

  # Okay now systemd
  # If we can talk to it. Since this is supposed to be a clean environment, don't re-use existing bus address.
  local bus=/run/user/$uid/bus
  if [[ -n $uid && -e $bus ]]; then
    local senv
    if senv=$(DBUS_SESSION_BUS_ADDRESS=unix:path=$bus systemctl --user show-environment 2>/dev/null); then
      local senvvar
      while IFS= read -r -d $'\n' senvvar; do
        local var=${senvvar%%=*}
        local val=${senvvar#*=}
        eval "val=$val" # val quoted by systemd, so we want to expand and eval it
        vars+=("$var=$val")
      done <<< "$senv"
    fi
  fi

  for var in "${vars[@]}"; do
    estat "$var"
  done
  local execshell && execshell=$(/bin/readlink -f /proc/$$/exe)
  cmd exec env -i "${vars[@]}" "$execshell"
}

nenv() {
  local env
  # If we don't even have a session bus try to find the default one for our user
  if [[ -z $(sh -c 'printf %s "${DBUS_SESSION_BUS_ADDRESS-}"') ]]; then
    local uid && uid=$(id -u)
    local bus=/run/user/$uid/bus
    [[ -n $uid && -e $bus ]] && export DBUS_SESSION_BUS_ADDRESS=unix:path=$bus
  fi

  if env=$(systemctl --user show-environment 2>/dev/null); then
    # systemctl outputs this in bash/zsh-compatible escaped format with one variable per line.  Prepend 'export' to each
    # line.  Careful of bash/zsh differences here
    env=${env//$'\n'/$'\n'export }
    env=${env:+export $env}
    eval "$env"
    einfo "New env: DISPLAY=${DISPLAY:-<empty/unset>} WAYLAND_DISPLAY=${WAYLAND_DISPLAY:-<empty/unset>}"
    # Now reload zsh since we just stomped PATH etc
    reload
  fi
}

# Re-execute or switch shell (to load new configs or whatever clearly)
reload() {
  # $SHELL is the user's login shell, not what is currently running interactively.
  local shell && shell=$(readlink -f /proc/$$/exe);
  local args
  # If we're a "login" shell use -l to re-exec too
  [[ $- = *l* ]] && args="-l"
  cmd exec -- "$shell" $args
}
rezsh() { cmd exec zsh; }
rebash() { cmd exec bash; }

pic() { s gwenview "$@"; }

gdiff() { cmd git diff --no-index "$@"; }
pd() { git diff --no-index --color=always "$@" | diff-so-fancy | less --tabs=4 -RFX; }

rv() { cmd rsync -avy --progress "$@"; }
rvp() { cmd rsync -avy --partial --inplace --progress "$@"; }

# Shorthand rdp
rdp()
{
  cmd xfreerdp /dynamic-resolution /scale-desktop:140 /scale:140 /scale-device:140 /clipboard /w:1920 /h:1200 /v:"$1" "${@:2}"
}

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

# Print a timestamp or timestamp for a date
tt() {
  local date
  if [[ $# -eq 0 ]]; then
    date="now"
  elif [[ $# -eq 1 && $1 =~ ^[0-9]+$ ]]; then
    date="@$1"
  else
    # Treat as date
    date="$*"
  fi
  einfo "Parsed"
  date --date="$date" || return
  einfo "Timestamp"
  date --date="$date" +%s
}

# Include local aliases
[[ ! -f $HOME/.aliases.local.sh ]] || source "$HOME"/.aliases.local.sh
