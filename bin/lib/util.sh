#!/bin/bash

#
# These utilities try to remain compatible with both bash+zsh currently with minimal divergance.
#
n_is_bash() { [[ -n ${BASH-} && -n ${BASH_VERSION-} ]]; }
n_is_zsh() { [[ -n ${ZSH_NAME-} && -n ${ZSH_VERSION-} ]]; }

# n_readvar varname
#   Read stdin into given variable name.  use n_herevar if you are using <<EOF to strip the trailing newline.
n_readvar() { IFS= read -r -d $'\0' -- "$*" ||:; }
# n_herevar varname
#   Read stdin into the given variable name, and strip trailing newlines.  Useful for using here-documents (<<EOF) to
#   load text into variables without weird syntax.
#
# Ex:
#   n_readvar foo <<EOF
#   Foo contents!
#   EOF
# Equivalent to:
#   foo="Foo contents!"
n_herevar() { local x; n_readvar x && cmd typeset -g "$1=${x%$'\n'}"; }

# Find currently set hostname if possible, trying tools for a few different environments.
#
# Some minimal linux systems, especially containers, but also e.g. a minimal Arch install, don't have a `hostname`
# binary.  I think BSD environments almost always will.
#
# The culprit is probably the systemd `hostnamectl` being a replacement -- except it cannot function if systemd is not
# init, so various container/chroot environments remain a problem.  Why that tool would not fall back to the system call
# so it was a proper superset of `hostname` is beyond me.
#
# We can read from /proc on linux hosts which saves us in most minimal-container situations.  Your weird cygwin
# environment without a hostname binary might not be covered by this -- patches welcome for other obscure cases.
n_hostname() {
  [[ $# -eq 0 ]] || eerr "Internal error: n_hostname takes no arguments"
  if command -v hostname &>/dev/null; then
    # Everyone should have this but many containers don't
    # I think OS X/BSDs will almost always though.
    hostname
  elif [[ -e /proc/sys/kernel/hostname ]]; then
    # Linux systems/containers without hostname command
    cat /proc/sys/kernel/hostname
  elif command -v hostnamectl &>/dev/null && systemctl status &>/dev/null; then
    # Systemd/hostnamectl.  This command will fail if systemd isn't init (containers)
    # But are there even systemd systems that aren't linux or work without /proc?
    hostnamectl hostname
  else
    eerr "Internal script error: Do not know how to lookup hostnames on this system"
    return 1
  fi
}

rightPad() {
  local padding=$1
  local str="${*:2}"
  local pad=$(( padding - ${#str} ))
  printf "%s%${pad}s" "$str" ""
}

formatBytes() {
  local bytes="$1"
  local padding=$2
  [[ ! -z "$padding" ]] || padding=0

  local num=$(( bytes + 0 ))
  local div=1
  local units=""
  if [[ $num -ge $(( 1024 ** 4 )) ]]; then
    div=$(( 1024 ** 4 ))
    units="TiB"
  elif [[ $num -ge $(( 1024 ** 3 )) ]]; then
    div=$(( 1024 ** 3 ))
    units="GiB"
  elif [[ $num -ge $(( 1024 * 1024 )) ]]; then
    div=$(( 1024 * 1024 ))
    units="MiB"
  elif [[ $num -ge $(( 1024 )) ]]; then
    div=1024
    units="KiB"
  fi

  local ret
  if [[ -z "$units" ]]; then
    ret="$num"B
  else
    ret=$(( ( num * 100 ) / div ))
    local ind=$(( ${#ret} - 2 ))
    ret="${ret:0:$ind}.${ret:$ind}${units}"
  fi

  out_raw "$ret"
  local pad=$(( padding - ${#ret} ))
  if [[ $pad -gt 0 ]]; then
    printf "%${pad}s" " "
  fi
  out ""
}

# n_assoc2json <array_ref>
#   Takes the name of an associative array and prints a json object
#
# Ex:
#   declare -A foo
#   foo[a]="A"
#   foo[b]="B"
#   n_assoc2json foo
# Prints:
#   {"a":"A","b":"B"}
n_assoc2json() {
  local arr_ref="$1"
  # shellcheck disable=SC2016 # (jq variables, intended that they're in single quotes)
  local script='
    # input is keys followed by values joined with NUL separators
    # and has extra trailing \0 for simplicity
    split("\u0000")[:-1]
    | length/2 as $len
    | .[:$len] as $keys
    | .[$len:] as $values
    | reduce range($len) as $idx ({}; .[$keys[$idx]] = $values[$idx])
  '
  if n_is_zsh; then
    # k = keys, P = deref
    # shellcheck disable=all # zsh syntax
    jq -McRs "$script" < <(printf "%s\0" ${(kP)arr_ref[@]} ${(P)arr_ref[@]})
  else
    local -n arr="$arr_ref"
    jq -McRs "$script" < <(printf "%s\0" "${!arr[@]}" "${arr[@]}")
  fi
}

# FIXME does not work in zsh, no namerefs
n_json2assoc() {
  local -n assoc=$1
  local json=$2

  local k v
  while IFS= read -r -d $'\0' k; do
    IFS= read -r -d $'\0' v ||:
    assoc["$k"]=$v
  done < <(jq --raw-output0 "to_entries.[] | .key, .value" <<< "$json")
}

# n_json_array [item [item...]]
#   Emits arguments as a JSON array
n_json_array() {
  jq -McRs 'split("\u0000")[:-1]' < <(printf "%s\0" "$@")
}

# n_array2json <array_ref>
#   Given the name of an array, output it as a JSON array
#
# See also n_json_array
n_array2json() {
  local array_ref=$1
  if n_is_zsh; then
    # shellcheck disable=all # zsh syntax
    n_json_array ${(P)array_ref[@]}
  else
    local -n arr=$array_ref
    n_json_array "${arr[@]}"
  fi
}

# n_validnum <input> [input...]
#
# Checks that the input is comprised of digits, in its canonical base-ten representation for the current shell, and in
# the valid range of integer inputs for the current shell.
#
# Notable: '0123' is rejected as non-canonical. Bash parses 0123 as 81b10 while zsh parses it as 123b10, bad
#
# Notable 2: zsh reports "number truncated after 18 digits" for -9223372036854775808 on 64bit platforms, even though
#            they can be _computed_ in math expressions (2**63), which appears to be a zsh bug.  This function returns
#            false there, as you cannot work with that number as an input.
#
#            19 digit inputs that are in the range of [INT_MIN+1, INT_MAX] _are_ valid and do not trigger said error
n_validnum() {
  local input
  for input in "$@"; do
  # Non-alphanumeric input like `1e+100` or `1.2` can round-trip through zsh but are treated as floats
  #
  # "0123" or "" blocked by this regex but would fail the roundtrip test regardless.
    [[ $input =~ ^[1-9][0-9]*$ && $(( input + 0 )) = "$input" ]] || return 1
  done
}

# n_commonword [count(=1) [min_length(=3) [max_length(=5)]]]
#
# outputs a word from /usr/share/dict/words
#
# word must be between the given lengths and be soley comprised of lowercase latin letters
#
# !! in some languages that constraint may yield few or no available words
#
# Returns code 1 with no output if there is not dictionary available or it does not contain sufficient words to fulfill
# request.
n_commonword() {
  local words
  local count="${1-1}"
  local min="${2-3}"
  local max="${3-5}"
  (n_validnum "$count" "$min" "$max" && (( min <= max ))) || die "invalid arguments to n_commonword"
  local wordsfile
  # Try just 'words' symlink first, otherwise some englishish stuff
  for candidate in /usr/share/dict/{words,usa,american-english,british-english,british}; do
    einfo "Checking $candidate"
    [[ -f $candidate ]] || continue
    wordsfile=$candidate
    break
  done
  if [[ ! -n $wordsfile ]]; then
    eerr "Couldn't find any /usr/share/dict/words style wordlists on this system"
    return 1
  fi
  words="$(cat -- "$wordsfile" | grep -E "^[a-z]{$min,$max}$" | shuf -n"$count")"
  if n_is_zsh; then
    words=("${(f)${words:-}}")
  else
    read -a -r -d'' words <<< "$words"
  fi
  [[ ${#words[@]} -eq $count ]] || return 1
  printf "%s\n" "${words[@]}"
}

eprompt() {
  local msg="$1"
  [[ -n $msg ]] || msg="?"
  ewarnprompt "$msg "
  local reply
  read -r reply
  info "" # Clear prompt line
  printf "%s" "$reply"
}

eprompt_yn() {
  local msg="$1"
  [[ -n $msg ]] || msg="Proceed?"
  ewarnprompt "$msg [y/N] "
  local reply
  if n_is_zsh; then
    read -k 1 -r reply
  else
    read -n 1 -r reply
  fi
  info "" # Clear prompt line
  [[ $reply = y || $reply = Y ]] || return 1
}

promptcmd() {
  emsg "Will execute:"
  emsg "  $(sh_quote "$@")"
  eprompt_yn 'Continue?' || return 1
  cmd "$@"
}

# Tries to connect to keychain, invalidates SSH_AGENT vars if it cannot.
try_keychain() {
  keyfile="$HOME/.keychain/$HOSTNAME-sh"
  [ ! -f "$keyfile" ] || source "$keyfile"
  if [ -z "$SSH_AUTH_SOCK" ] || \
     [ -z "$SSH_AGENT_PID" ] || \
     [ ! -e "$SSH_AUTH_SOCK" ] || \
     [ -z "$(ps -p "$SSH_AGENT_PID" -o comm=)" ]; then
    unset SSH_AUTH_SOCK
    unset SSH_AGENT_PID
    return 1
  fi
  return 0
}

##
## Some shorthand for common things that need to be typed differently in zsh/bash, for portable aliases and such
##
n_tolower() { if n_is_zsh; then out "${@:l}"; else out "${@,,}"; fi; }
n_toupper() { if n_is_zsh; then out "${@:u}"; else out "${@^^}"; fi; }

# Returns false if any argument is an unset variable
n_isset() {
  local var;
  for var in "$@"; do
    if n_is_zsh; then
      [[ -n ${(P)var+x} ]] || return 1
    else
      [[ -n ${!var+x} ]] || return 1
    fi
  done
  return 0
}

# Returns true if any of the passed variable names is set
n_anyset() {
  local var;
  for var in "$@"; do
    if n_is_zsh; then
      [[ -z ${(P)var+x} ]] || return 0
    else
      [[ -z ${!var+x} ]] || return 0
    fi
  done
  return 1
}

_sh_c_colors=0
# Some versions of `tput` get upset if you query it with an empty $TERM or "dumb", rather than just giving you the
# no-capabilities answer.
#
# See https://no-color.org for NO_COLOR. That owner of that domain really dislikes colors on their lawn.
if [[ -z ${NO_COLOR-} && -n ${TERM-} && $(n_tolower "${TERM-}") != dumb ]]; then
  _sh_c_colors="$(tput colors 2>/dev/null || echo 0)"
fi
_sh_c()
{
  [[ $_sh_c_colors -gt 0 ]] || return
  local args=("$@")
  [[ ${#args[@]} -gt 0 ]] || args=(0)
  ( IFS=\; && out_raw $'\e['"${args[*]}m"; );
}

sh_c() { [[ ! -t 2 ]] || _sh_c "$@"; }
sh_c_stdout() { :; }
[[ -t 1 ]] && sh_c_stdout() { _sh_c "$@"; }

# like echo >&2 without the pitfalls
info_raw() { printf >&2 "%s" "$*"; }
info() { printf >&2 "%s\n" "$*"; }

# like echo without the pitfalls
out_raw() { printf "%s" "$*"; }
out() { printf "%s\n" "$*"; }

estat()   { info "$(sh_c 32 1)::$(sh_c) $*"; }
estat2()  { info "   $(sh_c 34 1)->$(sh_c) $*"; }
emsg()    { info "$(sh_c 34 1)::$(sh_c) $*"; }
emsg2()   { info "   $(sh_c 34 1)->$(sh_c) $*"; }
ewarn()   { info "$(sh_c 33 1);;$(sh_c) $*"; }
ewarn2()  { info "   $(sh_c 33 1)=>$(sh_c) $*"; }
einfo()   { info "$(sh_c 30 1)::$(sh_c) $*"; }
einfo2()  { info "   $(sh_c 30 1)->$(sh_c) $*"; }
eerr()    { info "$(sh_c 31 1)!!$(sh_c) $*"; }
eerr2()   { info "   $(sh_c 31 1)~>$(sh_c) $*"; }
ewarnprompt() { info -n "$(sh_c 33 1)?$(sh_c) $*"; }
_elines() { local x; for x in "${@:2}"; do "$1" "$x"; done; }
_eblock() { _elines "$1" "" "${@:2}" ""; }
_epipe() { [[ $# -eq 1 ]] || return 1; local line; while IFS= read -r -d $'\n' line; do "$1" "$line"; done; }
_esplitlines() { _epipe "$1" <<< "${*:2}"; }
# Block variant -- emit each argument as its own line, with an empty line on either side for padding/call-out.
#                  That is, `foo_block a b` -> `foo; foo a; foo b; foo`
estat_block() { _eblock estat "$@"; }
emsg_block()  { _eblock emsg  "$@"; }
ewarn_block() { _eblock ewarn "$@"; }
einfo_block() { _eblock einfo "$@"; }
eerr_block()  { _eblock eerr  "$@"; }
# Lines variants -- emit each argument as its own line. That is, `foo_lines a b` -> `foo a; foo b`
estat_lines() { _elines estat "$@"; }
emsg_lines()  { _elines emsg  "$@"; }
ewarn_lines() { _elines ewarn "$@"; }
einfo_lines() { _elines einfo "$@"; }
eerr_lines()  { _elines eerr  "$@"; }
# Splitlines variants -- Raw newlines in input are split into separate calls, giving each line its own decorator.
#                        Multiple arguments are joined with a space prior to splitting as with base call.
estat_splitlines() { _esplitlines estat "$@"; }
emsg_splitlines()  { _esplitlines emsg  "$@"; }
ewarn_splitlines() { _esplitlines ewarn "$@"; }
einfo_splitlines() { _esplitlines einfo "$@"; }
eerr_splitlines()  { _esplitlines eerr  "$@"; }
# Pipe variants -- Read newline-delimited text from stdin and emit each line as its own message, with its own decorator
estat_pipe() { _epipe estat; }
emsg_pipe()  { _epipe emsg; }
ewarn_pipe() { _epipe ewarn; }
einfo_pipe() { _epipe einfo; }
eerr_pipe()  { _epipe eerr; }
# Title variants -- emits an empty line on either side for padding/call-out.
#                   That is, `foo_title a b` -> `foo; foo a b; foo`
estat_title() { _eblock estat "$*"; }
emsg_title()  { _eblock emsg  "$*"; }
ewarn_title() { _eblock ewarn "$*"; }
einfo_title() { _eblock einfo "$*"; }
eerr_title()  { _eblock eerr  "$*"; }
eerrint() { eerr "$@"; return 1; }

edivider() {
  local n="$1"
  local char="$2"
  [[ $n =~ ^[0-9]+$ ]] || n=3
  [[ -n $char ]] || char=-

  local run="$((n / ${#char}))"
  local change="$(( n - ( run * ${#char} ) ))"

  local line
  # {1..$run} isn't a thing in bash, so eval.
  # w/run=10, expands to: `printf -- "%.${#char}s" "$char"{1..10}`
  [[ $run -lt 1 ]] || line="$(eval 'printf -- "%.0${#char}s" "$char"'"{1..$run}")"
  local tail="${char[*]:0:$change}"
  info "$(sh_c 90)${line}${tail}$(sh_c)"
}

# Shows "+ command" as stderr, info style
showcmd() { showcmd_unquoted "$(sh_quote "$@")"; }
# Shows "+ command" but unquoted, e.g. for displaying things that are going to be eval'd or where
# you are manually formatting the displayed command.
showcmd_unquoted() { printf "%s\n" >&2 "$(sh_c 30 1)+$(sh_c) $*"; }
# Shows "`#` command" as stdout, copy-pasteable by user (`#` is a bash no-op)
offercmd() { printf "%s\n" "$(sh_c 30 1)\`#\`$(sh_c) $(sh_quote "$@")"; }
# showcmd and also actually run it
cmd() { showcmd "$@"; "$@"; }
# showcmd and actually run it, with stderr to /dev/null. This is helpful since
#   showcmd echos to stderr, so $(cmd 2>/dev/null ...) is self-defeating
scmd() { showcmd "$@"; "$@" 2>/dev/null; }
# showcmd and actually run it, with stdout to /dev/null
qcmd() { showcmd "$@"; "$@" >/dev/null; }
# showcmd and actually run it, with all output to /dev/null
qqcmd() { showcmd "$@"; "$@" &>/dev/null; }
# eval quoted command, showing the eval'd version
ecmd() { showcmd_unquoted "$@"; eval "$@"; }

die() { local msg="$*"; [[ -n $msg ]] || msg="script terminated"; eerr_title "$msg"; exit 1; }
_assert_failed() { die "Assertion failed${*+:: $*}"; }
assert_test() { test "$@" || _assert_fail; }
assert_cmd() { cmd "$@" || _assert_fail; }
assert_value()
{
  local val; for val in "$@"; do
    [[ -n $val ]] || _assert_fail
  done
}

# Internal error to util.sh
_interr() { die "Internal script error${*:+: $*}"; }

sh_is_callable_thing()
{
  if [[ $# -ne 1 ]]; then
    eerr "Internal error: sh_is_callable_thing takes exactly 1 argument"
    return 1
  fi

  local cmd="$1"
  case "$([[ -z $cmd ]] || type -t "$cmd" || true)" in
    "function" |\
    "builtin" |\
    "alias" |\
    "file")
      return 0
    ;;
    *)
      return 1
    ;;
  esac
}

require_commands()
{
  if [[ $# -lt 2 ]]; then
    eerr "Internal error: require_commands mis-used, requires at least two arguments"
    return 1
  fi

  local script="$1"
  shift
  local cmd
  while [[ $# -gt 0 ]]; do
    cmd="$1"
    shift
    if ! sh_is_callable_thing "$cmd"; then
      eerr "'$cmd' utility not found, required by '$script'"
      return 1
    fi
  done
  return 0
}

sh_quote()
{
  local q=()
  if n_is_zsh; then
    # zsh just has a readable quoting mode. See also (q) (q-)

    # shellcheck disable=2296 disable=2206 # this is zsh, shellcheck
    q+=(${(q+)@})
  else
    # Bash's @Q 'quotes' 'everything' 'simple' 'thing'
    # but printf %q uses\ backslashes\ everywhere and also isn't great.
    # To get readable quoting, first check if printf %q outputs the bare word and otherwise use @Q
    local q=()
    local i
    local o
    for i in "$@"; do
      o="$(printf "%q" "$i")"
      [[ $o = "$i" ]] || o="${i@Q}"
      q+=("$o")
    done
  fi
  printf "%s\n" "${q[*]}"
}

# Prints eval-able expression to set given variable, e.g.:
# sh_var DISPLAY -> "DISPLAY=':0'"
sh_var()
{
  declare -n ref="$1" || return 1
  printf "%s\n" "$(sh_quote "$1")"="$(sh_quote "$ref")"
}

parse_args()
{
  if [[ $# -lt 3 ]]; then
    eerr "Internal error: incorrect usage of parse_args"
  fi

  _parse_args_options=()
  _parse_args_values=()
  _parse_args_args=()
  [[ $# -gt 3 ]] || return 0

  local app_name="$1"
  local short_opts="$2"
  local long_opts="$3"

  shift; shift; shift

  local parsed

  if ! parsed="$(getopt -n "$app_name" -o "$short_opts" -l "$long_opts" -- "$@")"; then
    return 1
  fi
  eval parsed=\("$parsed"\)
  local i=0
  while [ $i -lt ${#parsed[@]} ]; do
    local arg="${parsed[$i]}"
    local opt
    local val
    (( ++i ))
    if [ ${#arg} -lt 2 ] || [ "${arg:0:1}" != "-" ]; then
      eerr "Internal error: Unexpected arg \"$arg\" while parsing getopt output"
      continue
    fi
    if [ "$arg" = "--" ]; then # end of options
      _parse_args_args=("${parsed[@]:$i}")
      break
    elif [ "${arg:1:1}" = "-" ]; then # long opt
      local opt_name="${arg:2}"
      local IFS=","
      for x in ${long_opts}; do
        if [ "${x:$(( ${#x} - 1 ))}" = ":" ] &&
           [ "${x:0:$(( ${#x} - 1 ))}" = "$opt_name" ]; then
          # arg long opt
          opt="$opt_name"
          val="${parsed[$i]}"
          (( ++i ))
          opt_name=""
          continue
        elif [ "$x" = "$opt_name" ]; then
          # boolean long opt
          opt="$opt_name"
          val=1
          opt_name=""
          continue
        fi
      done
      if [ ! -z "$opt_name" ]; then
        eerr "Internal error: Failed to find opt \"$opt_name\""
      fi
    elif [ ${#arg} -eq 2 ]; then # short opt
      local shorts="$short_opts"
      while [ ! -z "$shorts" ] && [ "${shorts:0:1}" != "${arg:1:1}" ]; do
        shorts="${shorts:1}"
      done
      if [ -z "$shorts" ]; then
        eerr "Internal error: Unfound short option $arg"
      else
        if [ "${shorts:1:1}" = ":" ]; then
          # arg short opt
          opt="${arg:1:1}"
          val="${parsed[$i]}"
          (( ++i ))
        else
          # bool short opt
          opt="${arg:1:1}"
          val=1
        fi
      fi
    fi
    _parse_args_options[${#_parse_args_options[@]}]="$opt"
    _parse_args_values[${#_parse_args_values[@]}]="$val"
  done
}

# get_merged_option option_one [option_two ...]
#   For aliased options, e.g. -h and --help, gets first set value (pass options
#   in descending precedence)
get_merged_option()
{
  local opt_name
  local i=0
  while [ $i -lt ${#_parse_args_options[@]} ]; do
    for opt_name in "$@"; do
      if [ "${_parse_args_options[$i]}" = "$opt_name" ]; then
        out "${_parse_args_values[$i]}"
        return
      fi
    done
    (( ++i ))
  done
}

get_option()
{
  local opt_name="$1"
  local default="${2-}"
  local i=0
  while [[ $i -lt ${#_parse_args_options[@]} ]]; do
    if [[ ${_parse_args_options[$i]} = $opt_name ]]; then
      out "${_parse_args_values[$i]}"
      return
    fi
    (( ++i ))
  done
  out "$default"
}

# Differentiate between empty option and not passed
has_option()
{
  local opt_name="$1"
  local opt
  for opt in "${_parse_args_options[@]}"; do
    [[ $opt != $opt_name ]] || return 0
  done
  # Not found
  return 1
}

# Like has_option, but true if any one of these exists
has_merged_option()
{
  local opt
  for opt in "$@"; do
    has_option "$opt" && return 0
  done
  return 1
}

get_arg()
{
  # Args are 1-indexed, though we don't keep $0 around
  local i=$(($1 - 1))
  out "${_parse_args_args[$i]}"
}

num_args()
{
  out "${#_parse_args_args[@]}"
}

get_args()
{
  sh_quote "${_parse_args_args[@]}"
}

## FIXME WIP
##
## Guided script - lets you push some commands with comments and then execute them or view them
##
## Each item pushed can be:
##  - cmd <arg> [arg...]: runs a command with the 'cmd' function (in run mode) or shows it with 'showcmd' in display mode
##                        mode.  Hint: use `cmd eval "foo | bar"` for nested pipelines/indirection/etc.
##  - comment <message>: A comment displayed between running or showing commands.
##
## Example:
##   # Setup a set of commands in variable mycmds
##   unset mycmds
##   n_cmds mycmds comment "Make directory and enter"
##   n_cmds mycmds cmd mkdir foo
##   n_cmds mycmds cmd cd foo
##   n_cmds mycmds comment "Touch a sentinel file"
##   n_cmds mycmds cmd touch ./.created_by_script_or_something
##
##   # Show a proposed script to the user
##   estat "Would run the following"
##   n_cmds_show mycmds
##   # Offer to run them now
##   eprompt_yn "Proceed?" && n_cmds_run mycmds
# n_cmds() {
#   [[ $# -ge 2 ]] || _interr "Invalid usage of n_cmds, too few arguments (args were: ${*@Q})"
#   declare -Anl -- var="$1"         || _interr "Invalid variable named passed to n_cmds"
#   local newlen=$(( var[len] + 1 )) || _interr
#   var[$newlen]=("${*:2@Q}")        || _interr
#   var[len]=$newlen                 || _interr
# }
#
# _n_docmds() {
#   local doit=$1
#   local cmds=("${@:2}")
#   for c in "${cmds[@]}"; do
#     # FIXME
#     eval c="($c)"
#     type="${c[0]}"
#     if [[ $type = comment ]]; then
#       einfo "${c[@]:1}"
#     elif [[ $type = cmd ]]; then
#       if [[ -n $doit ]]; then
#         cmd "${c[@]:1}"
#       else
#         showcmd "${c[@]:1}"
#       fi
#     fi
#   done
# }
#

# Unset internal functions
unset _interr
