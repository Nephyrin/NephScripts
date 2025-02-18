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
# The s() alias below is often a better way to do this in a systemd system.
x() {
  # Warn if you're accidentally launching something in a cgroup (since that is not detached) using the neph cgroup
  # functions.
  [[ -z $NEPH_CGROUP ]] || ewarn "WARNING: In cgroup"

  # If `command -v` doesn't think this parses as something runnable, just run it bare so the shell-level
  # error/error-code occurs.  This means that `x some_typo --args` doesn't silently succeed, but errors exactly as
  # `some_typo --args` would.
  #
  # See warning above -- some classes of command will silently fail
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
  # --property=ExitType=cgroup - Don't tear down the whole tree when the initial pid exits
  cmd systemd-run --user --same-dir --collect --property=ExitType=cgroup -- "$@"
}

# spawns a detached command with systemd-run, but *without* preserving working directory.
# This is just s() without `--same-dir`
#
# This is useful for spawning long-running things that don't care about working directory, without accidentally keeping
# a reference to random temp directories or external mounts etc related to that process.
S() {
  cmd systemd-run --user --collect --property=ExitType=cgroup -- "$@"
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

brs() { s dolphin -- "$@"; }

# enhanced reset
#  - rmcup exits alternate screen mode that utilities leave on. In theory typing this in some nested bash-in-ncurses app
#    might break it, but things like tmux emulate the nested terminal, so you're not breaking the outer tmux session.
#    Fixes panes in tmux randomly not scrolling correctly anymore (since all shell interaction is on the alternate
#    screen and not contributing to scrollback)
ereset() {
  cmd reset
  cmd tput rmcup
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

# nenv
#   check for a systemd user session and import its environment, then re-exec/reload the shell
#   useful for e.g. acquiring DISPLAY from an ssh environment or desync'd tmux session
#
# see also `systemd-run --user --shell`, which is going to connect you to a scope'd unit/shell in that environment
nenv() {
  if [[ $# -ne 0 ]]; then eerr "nenv takes no arguments"; return 1; fi

  # If we don't even have a session bus try to find the default one for our user
  #
  # XXX: I don't remember why I added this, `systemctl --user` will try the default path already. It may not have
  #      previously.

  # if [[ -z ${DBUS_SESSION_BUS_ADDRESS-} ]]; then
  #   local uid && uid=$(id -u)
  #   local bus=/run/user/$uid/bus
  #   [[ -n $uid && -e $bus ]] && export DBUS_SESSION_BUS_ADDRESS=unix:path=$bus
  # fi

  local env
  if env=$(systemctl --user show-environment 2>/dev/null); then
    # systemctl outputs this in bash/zsh-compatible escaped format with one variable per line.  Prepend 'export' to each
    # line.  Careful of bash/zsh differences here
    env=${env//$'\n'/$'\n'export }
    env=${env:+export $env}
    eval "$env"
    einfo "New env: DISPLAY=${DISPLAY:-<empty/unset>} WAYLAND_DISPLAY=${WAYLAND_DISPLAY:-<empty/unset>}"
    # Now re-exec shell since we just stomped PATH etc
    reload
  fi
}

# use wl-copy to throw a file on the clipboard as a file:// uri, which will be handled by most things that handle random
# pastes of images/videos/etc..
#
# Doesn't try to also provide it as a full video/mp4 and etc etc
uricopy() {
  local file && file=$(realpath -e -- "$1")
  local uri && uri="file://$file"
  einfo "copying $uri"
  printf %s "$uri" | wl-copy -t text/uri-list
}

# Re-execute or switch shell (to load new configs or whatever clearly)
#
# If no shell is provided, re-execs current shell
reload() {
  local shell
  if [[ $# -ge 1 ]]; then
    shell=$(command -v "$1")
    if [[ -z $shell || ${shell:0:1} != / ]]; then
      eerr "Given shell '$1' doesn't resolve to a binary"
      return 1
    fi
  else
    # note that $SHELL is the user's login shell, not what is currently running interactively
    shell=/proc/$$/exe
  fi
  if ! shell=$(cmd realpath -e -- "$shell"); then
    eerr "Couldn't resolve shell to execute"
    return 1
  fi
  local args
  # If we're a "login" shell use -l to re-exec too
  [[ $- = *l* ]] && args="-l"
  cmd exec -- "$shell" $args
}
rezsh() { cmd exec zsh; }
rebash() { cmd exec bash; }

pic() { s qimgv "$@"; }

rand32() { shuf -i 0-$(( 2**32 - 1 )) -n 1; }

gdiff() { cmd git diff --no-index "$@"; }
pd() { git diff --no-index --color=always "$@" | diff-so-fancy | less --tabs=4 -RFX; }

rv() { cmd rsync -avy --progress "$@"; }
rvp() { cmd rsync -avy --partial --inplace --progress "$@"; }

# Shorthand rdp
# Tries to find xfreerdp2 or 3 as a fallback (which performs much worse) and launches it with sane options
rdp()
{
  local rdpcmd
  local rdparg=()
  if command -v xfreerdp2 &>/dev/null; then
    rdpcmd=xfreerdp2
  elif command -v xfreerdp &>/dev/null; then
    rdpcmd=xfreerdp
  elif command -v xfreerdp3 &>/dev/null; then
    rdpcmd=xfreerdp3
  else
    eerr "Couldn't find xfreerdp binary"
    return 1
  fi

  if $rdpcmd --version 2>/dev/null | grep -q 'version 2'; then
    estat "Found xfreerdp v2.x, preferring"
    rdparg=(/gfx:AVC444)
  else
    # v3 lags and chugs 1000% CPU with gfx pipeline for some reason, prefer rfx. v2 is great at it...?
    estat "Couldn't find xfreerdp v2.x, assuming v3"
    # Also it tries to init kerberos which nobody wants
    rdparg=(/rfx /auth-pkg-list:'!kerberos')
  fi

  cmd $rdpcmd /dynamic-resolution /scale-desktop:140 /scale:140 /scale-device:140 /clipboard /w:1920 /h:1200 \
              "${rdparg[@]}" /v:"$1" "${@:2}"
}

winepids()
{
  local pids=($(egrep -lzEi '^WINELOADERNOEXEC=' /proc/*/environ 2>/dev/null \
                 | sed -r 's/^\/proc\/([0-9]+)\/environ$/\1/'))
  echo "${pids[@]}"
}


iswine()
{
  local winepids=($(winepids))
  if [[ ${#winepids[@]} -gt 0 ]]; then
    cmd ps -fp "${winepids[@]}"
  else
    ewarn No wine processes found
  fi
}

killwine()
{
  local winepids=($(winepids))
  if [[ ${#winepids[@]} -gt 0 ]]; then
    cmd kill "${1--9}" "${winepids[@]}"
  else
    ewarn No wine processes found
  fi
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
    if [[ ! -d $x || -L $x ]]; then
      ewarn "$x: Not a simple directory, ignoring"
      continue
    fi
    if findmnt -m "$x" &>/dev/null; then
      ewarn "$x: Appears to be a mount point, ignoring"
      continue
    fi

    local pids
    # +D    Scan directory hierarchy for open files
    # -x f  Also cross into mountpoints within
    # -Fp   Output pid
    #
    # TODO If there's any mounts in here we should probably not clean it up. --one-file-system will avoid accidentally
    #      nuking them, but we'll fail to finish cleaning the directory anyway.
    #
    # FIXME If something is mounted directly on the nephtmp.xxx folder, we're going to nuke it inappropriately I think.
    #  findmnt -T "$x"     should be null, don't touch mountpoints
    #  findmnt -T "$x" -R  Should find everything mounted under the same mount (probably /tmp) -- grep for our prefix?
    #                      Make sure we're not a symlink?
    pids=($(lsof -x f -Fp +D "$x" | tr -d p))
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

v() {
  local json
  if ! json=$(cmd yt-dlp --no-simulate -J "$@"); then
    eerr "yt-dlp failed, see above"
    return 1
  fi

  local filename
  filename=$(jq -r '.requested_downloads[0]._filename | select(type == "string")' <<< "$json")
  if [[ -z $filename ]]; then
    jq <<< "$json"
    eerr "yt-dlp didn't return a URL, see JSON dump above"
    return 1
  fi

  filename="file://$PWD/$filename"
  cmd systemd-run --user --collect -- bash -c "exec wl-copy --foreground -t 'text/uri-list' <<< $(sh_quote $filename)"
}

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

# quickly select a bunch of local git branches to nuke
git-fzf-delete-branches() {
  local branch
  for branch in $(git branch -l | grep -Eo '[^ ]+$' | fzf --multi); do
    cmd git branch -d -- "$branch"
  done
}

# Include local aliases
[[ ! -f $HOME/.aliases.local.sh ]] || source "$HOME"/.aliases.local.sh
