#!/bin/bash

# Shared file to be sourced in shell startup
# Meant to be compatible with zsh+bash

if [[ $- == *i* ]]; then # Only if interactive
  # Keychain
  _neph_keychain=~/".keychain/$(hostname)-sh"
  if command -v keychain &>/dev/null && [[ -f $_neph_keychain && -f ~/.ssh/id_rsa ]]; then
    #export SSH_ASKPASS=neph-askpass
    keychain --nogui ~/.ssh/id_rsa
    source "$_neph_keychain"
  fi
  unset _neph_keychain

  # Show mdstat
  [[ ! -f /proc/mdstat ]] || cat /proc/mdstat

  # Include shared util commands for interactive mode
  [[ ! -f $NEPH/bin/lib/util.sh ]] || source "$NEPH"/bin/lib/util.sh

  # Include aliases
  [[ ! -f $NEPH/aliases.sh ]] || source "$NEPH"/aliases.sh
fi
