# Setup:

# 1 - make a mozilla directory somewhere, with your source tree(s), and a "cfg"
#     directory containing mozconfigs, ending in .mzc
# 2 - In your bashrc, export MOZPATH to your mozilla directory and source moz.sh
# 3 - Optional, define a _reprompt command to rebuild your PS1 when $MOZ_PS1
#     changes

# # Example .bashrc:
# export MOZPATH="$HOME/moz"
# source "$HOME/moz/scripts/moz.sh"
#
# BASE_PS1="\u@\h\[\e[01;34m\] \w \$\[\e[00m\] "
# _reprompt() {
#   PS1="$MOZ_PS1$BASE_PS1"
# }
# _reprompt

#FIXME add default tree, document mozinfo, MOZGENCFG always
# Split out the "mb -b" stuff?

# COMMANDS
# moz <config> [<suffix> [<source tree>]]
#   - Selects a moz config, setting various env vars for other scripts to use
#     $MOZPATH   - Absolute path to base mozilla directory (actually set by you,
#                  above)
#     $MOZTREE   - Current source dir, relative to $MOZPATH
#     $MOZCFG    - Current mozconfig name
#     $MOZCONFIG - path to mozconfig, equal to "$MOZPATH/cfg/$MOZCFG.mzc" unless
#                  $MOZGENCFG is 1, see below
#     $MOZ_PS1   - A PS1 string describing your selected config, _reprompt will
#                  be called when this changes
#     $MOZGENCFG - If 1, a temporary config was generated and MOZCONFIG points
#                  to e.g. /tmp/tmpcfg.cfg, which would include the actual
#                  mozconfig (used for overriding objdect directories when using
#                  suffixes)
#     $MOZBUILDTREE - Where the build tree is created for the current config,
#                     relative to $MOZPATH
#     $MOZOBJ    - Where the object directory is relative to $MOZPATH
#
# mt
#   - Changes to the current source directory, e.g. cd "$MOZPATH/$MOZTREE"
# mo
#   - Changes to the current object directory, e.g. cd "$MOZPATH/$MOZOBJ"
# _reprompt
#   - If defined by your bashrc, will be called when MOZ_PS1 changes, see
#     example .bashrc above

# SUFFIXES
# The moz() function will create an object directory per config, but share a
# build directory. If you want to have multiple build directories, such as one
# for beta, you can use a "suffix", which will be appended to the build tree and
# object directory. e.g.:
#   $ moz dbg
#   MOZBUILDTREE=${MOZTREE}-build
#   MOZOBJ=dbg
#
#   $ moz opt
#   MOZBUILDTREE=${MOZTREE}-build
#   MOZOBJ=opt
#
#   $ moz dbg beta
#   MOZBUILDTREE=${MOZTREE}-build-beta
#   MOZOBJ=dbg-beta
#
#   $ moz opt beta
#   MOZBUILDTREE=${MOZTREE}-build-beta
#   MOZOBJ=opt-beta

source "$(dirname "${BASH_SOURCE[0]}")/util.sh"

_update_mozinfo() {
  if [[ -z "$MOZPATH" || -z "$MOZOBJ" ]]; then
    eerr "No mozconfig"
    return 1
  fi
  eval mozinfo=($(cat "$MOZPATH"/mozinfo))
  local i=-2
  while [ $(( i += 2 )) -lt ${#mozinfo[@]} ]; do
    obj="${mozinfo[$(( i + 1 ))]}"
    tree="${mozinfo[$i]}"
    if [ "$obj" != "$MOZOBJ" ]; then
      echo $(sh_quote "$tree") $(sh_quote "$obj") >> "$MOZPATH"/mozinfo.new
    fi
  done
  echo $(sh_quote "$MOZBUILDTREE") $(sh_quote "$MOZOBJ") >> "$MOZPATH"/mozinfo.new
  mv "$MOZPATH"/mozinfo.new "$MOZPATH"/mozinfo
}

moz_get_suffix_objdirs() {
  [[ -n "$MOZPATH" && -n "$MOZBUILDTREE" ]] || die "No moz config in env"

  local mozinfo="$MOZPATH"/mozinfo
  [ -r "$mozinfo" ] || return 0
  eval mozinfo=($(cat "$mozinfo"))
  local i=-2
  while [ $(( i += 2 )) -lt ${#mozinfo[@]} ]; do
    obj="${mozinfo[$(( i + 1 ))]}"
    tree="${mozinfo[$i]}"
    if [[ "$tree" = "$MOZBUILDTREE" && -d "$MOZPATH/$obj" ]]; then
      echo "$(printf "%q" "$obj")"
    fi
  done
}

# Sets the current moz config and target
moz() {
    if [ $# -gt 3 ]; then
      eerr "Usage: moz <config> [<suffix> [<source tree>]]"
      return 1
    fi
    if [ "0$MOZGENCFG" -eq 1 ] && [ -f "$MOZCONFIG" ]; then
        # MOZGENCFG means we're using a generated config file. Nuke it - we'll
        # remake it if neede.
        rm "$MOZCONFIG"
    fi
    if [ $# -eq 0 ]; then
        estat "Clearing moz config"
        unset MOZTREE
        unset MOZCFG
        unset MOZCONFIG
        unset MOZ_PS1
        unset MOZGENCFG
        unset MOZBUILDTREE
        unset MOZSUFFIX
        unset MOZOBJ
        _reprompt 2>/dev/null || true
        return
    fi
    mozfile="$(readlink -f "$MOZPATH/cfg/$1.mzc")"
    if [ ! -f "$mozfile" ]; then
      eerr "No config named $1"
      return 1
    fi
    MOZSUFFIX=""
    [ $# -lt 2 ] || MOZSUFFIX="$2"
    MOZCFG="$1"
    MOZOBJ="$MOZCFG"
    [ -z "$MOZSUFFIX" ] || MOZOBJ="$MOZOBJ-$MOZSUFFIX"
    if [ -f "$MOZPATH/$MOZOBJ/Makefile" ]; then
        # See if it has a tree
        configured_tree="$(egrep '^topsrcdir' "$MOZPATH/$MOZOBJ/Makefile" | awk '{ print $NF }')"
        configured_tree="${configured_tree##*/}"
        configured_tree="${configured_tree%-build*}"
    fi
    if [ $# -ge 3 ]; then
      MOZTREE="$3"
    elif [ ! -z "$configured_tree" ]; then
      MOZTREE="$configured_tree"
    elif [ ! -z "$MOZDEFAULTTREE" ]; then
      MOZTREE="$MOZDEFAULTTREE"
    else
      eerr "MOZDEFAULTTREE isn't set in your env, and no tree was specified"
      return 1
    fi
    MOZBUILDTREE="$MOZTREE-build"
    [ -z "$MOZSUFFIX" ] || MOZBUILDTREE="$MOZBUILDTREE-$MOZSUFFIX"
    if [ ! -f "$MOZPATH/$MOZTREE/client.mk" ]; then
        eerr "$MOZTREE does not appear to exist"
        return 1
    fi
    if [ ! -z "$configured_tree" ] && [ "$configured_tree" != "$MOZTREE" ]; then
        ewarn "$MOZOBJ is currently configured against tree $configured_tree"
    fi
    MOZCONFIG="$mozfile"
    MOZGENCFG=0
    local extraps1
    if [ ! -z "$MOZSUFFIX" ]; then
        extraps1="\[\e[0m\] / \[\e[0;31m\]$MOZSUFFIX"
    fi
    if [ "$MOZCFG" != "$MOZOBJ" ]; then
        MOZCONFIG="$(mktemp -t mozcfg.XXXX)"
        MOZGENCFG=1
        echo ". $mozfile" >> "$MOZCONFIG"
        echo "mk_add_options MOZ_OBJDIR=@TOPSRCDIR@/../$MOZOBJ" >> "$MOZCONFIG"
    fi
    # FIXME doesn't display non-default tree
    MOZ_PS1="\[\e[0;37m\][\[\e[0;33m\]$MOZCFG$extraps1\[\e[0;37m\]] "
    _reprompt 2>/dev/null || true
    _update_mozinfo
    export MOZCONFIG MOZTREE MOZCFG MOZOBJ MOZGENCFG MOZSUFFIX MOZBUILDTREE
}

# cd to moz objdir
mo() {
    if [ -z "$MOZCFG" ]; then
        eerr "No moz config"
        return 1
    fi
    if [ ! -d "$MOZPATH/$MOZCFG" ]; then
        eerr "The tree isn't built"
        return 1
    fi
    cd "$MOZPATH/$MOZCFG"
}

# cd to moz tree
mt() {
  if [ -z "$MOZPATH" ]; then
    eerr "No MOZPATH set in your env"
    return 1
  fi
  if [ -n "$MOZTREE" ]; then
    cmd cd "$MOZPATH/$MOZTREE"
  elif [ -n "$MOZDEFAULTTREE" ]; then
    cmd cd "$MOZPATH/$MOZDEFAULTTREE"
  else
    eerr "No moz config select, and MOZDEFAULTTREE isn't set in your env"
  fi
}
