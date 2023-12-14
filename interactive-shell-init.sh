#!/bin/bash

# Shared file to be sourced in shell startup
# Meant to be compatible with zsh+bash

if [[ $- == *i* ]]; then # Only if interactive
  # Common settings
  export EDITOR=ec
  export CCACHE_DIR=$HOME/.ccache
  export CCACHE_COMPRESS=1

  # fx
  export FX_THEME=3

  # Include shared util commands for interactive mode
  [[ ! -f $NEPH/bin/lib/util.sh ]] || source "$NEPH"/bin/lib/util.sh

  _inc_type=bash
  n_is_bash || _inc_type=zsh

  # git extras completions
  [[ ! -f /usr/share/doc/git-extras/git-extras-completion.$_inc_type ]] || source /usr/share/doc/git-extras/git-extras-completion.$_inc_type

  # Keychain
  _neph_keychain=~/".keychain/$(n_hostname)-sh" # n_hostname from util.sh since some things (containers) don't have
                                                # `hostname` binary
  if command -v keychain &>/dev/null && [[ -f $_neph_keychain && -f ~/.ssh/id_rsa ]]; then
    #export SSH_ASKPASS=neph-askpass
    #--confirm
    keychain --nogui ~/.ssh/id_rsa
    source "$_neph_keychain"
  fi
  unset _neph_keychain

  # Show mdstat
  [[ ! -f /proc/mdstat ]] || cat /proc/mdstat


  # Load fzf
  # FIXME Shouldn't this just be enabling the zsh fzf plugin?
  [[ ! -f /usr/share/fzf/key-bindings.$_inc_type ]] || source /usr/share/fzf/key-bindings.$_inc_type
  [[ ! -f /usr/share/fzf/completion.$_inc_type ]] || source /usr/share/fzf/completion.$_inc_type
  # The fzf scripts assume these are bound, don't piss off set -u mode.
  FZF_CTRL_T_OPTS=;
  export FZF_DEFAULT_OPTS="--margin 0,2% --border --height=~40%"
  # Give fzf ctrl-t a bat preview if bat is available
  ! type bat &>/dev/null || export FZF_CTRL_T_OPTS="--preview '[[ ! -f {} ]] || bat --color=always {} --style=header-filesize'"
  ! type rg &>/dev/null || export FZF_DEFAULT_COMMAND="rg --files --no-ignore-vcs --hidden"
  ! type rg &>/dev/null || export FZF_CTRL_T_COMMAND="rg --files --no-ignore-vcs --hidden 2>/dev/null"
  ! type fd &>/dev/null || export FZF_ALT_C_COMMAND="fd -u -t d"

  # Enable a floating pane in tmux mode
  # export FZF_TMUX_OPTS='-p90%,40% -x 0% -y 100%'

  # foo
  neph-fzf-cd-history-widget() {
    local cmd="dirs -v"
    setopt localoptions pipefail no_aliases 2> /dev/null
    local dir="$(eval "$cmd" | FZF_DEFAULT_OPTS="--height ~40% --reverse --scheme=path --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} ${FZF_ALT_C_OPTS-}" $(__fzfcmd) +m)"
    if [[ -z "$dir" ]]; then
      zle redisplay
      return 0
    fi
    zle push-line # Clear buffer. Auto-restored on next prompt.
    BUFFER="builtin cd -- -${dir%%$'\t'*}"
    zle accept-line
    local ret=$?
    unset dir # ensure this doesn't end up appearing in prompt expansion
    zle reset-prompt
    return $ret
  }
  zle     -N             neph-fzf-cd-history-widget
  bindkey -M emacs '\er' neph-fzf-cd-history-widget
  bindkey -M vicmd '\er' neph-fzf-cd-history-widget
  bindkey -M viins '\er' neph-fzf-cd-history-widget

  unset _inc_type
fi
