#!/bin/bash

# Shared file to be sourced in shell startup
# Meant to be compatible with zsh+bash

if [[ $- == *i* ]]; then # Only if interactive
  # Common settings
  export EDITOR=ec
  export CCACHE_DIR=$HOME/.ccache
  export CCACHE_COMPRESS=1

  # Keychain
  _neph_keychain=~/".keychain/$(hostname)-sh"
  if command -v keychain &>/dev/null && [[ -f $_neph_keychain && -f ~/.ssh/id_rsa ]]; then
    #export SSH_ASKPASS=neph-askpass
    #--confirm
    keychain --nogui ~/.ssh/id_rsa
    source "$_neph_keychain"
  fi
  unset _neph_keychain

  # Show mdstat
  [[ ! -f /proc/mdstat ]] || cat /proc/mdstat

  # Include shared util commands for interactive mode
  [[ ! -f $NEPH/bin/lib/util.sh ]] || source "$NEPH"/bin/lib/util.sh

  # Load fzf
  # FIXME Shouldn't this just be enabling the zsh fzf plugin?
  _fzf_type=bash
  n_is_bash || _fzf_type=zsh
  [[ ! -f /usr/share/fzf/key-bindings.$_fzf_type ]] || source /usr/share/fzf/key-bindings.$_fzf_type
  [[ ! -f /usr/share/fzf/completion.$_fzf_type ]] || source /usr/share/fzf/completion.$_fzf_type
  # The fzf scripts assume these are bound, don't piss off set -u mode.
  FZF_CTRL_T_OPTS=; FZF_DEFAULT_OPTS=
  # Give fzf ctrl-t a bat preview if bat is available
  ! type bat &>/dev/null || FZF_CTRL_T_OPTS="--preview '[[ ! -f {} ]] || bat --color=always {}'"
  ! type rg &>/dev/null || export FZF_DEFAULT_COMMAND="rg --files --no-ignore-vcs --hidden"

  unset _fzf_type
fi
