#!/bin/bash

# Shared file to be sourced in shell startup
# Meant to be compatible with zsh+bash

if [[ $- == *i* ]]; then # Only if interactive
  # Keychain
  _neph_keychain=~/".keychain/$(hostname)-sh"
  if command -v keychain &>/dev/null && [[ -f $_neph_keychain && -f ~/.ssh/id_rsa ]]; then
    export SSH_ASKPASS=neph-askpass
    keychain --confirm --nogui ~/.ssh/id_rsa
    source "$_neph_keychain"
  fi
  unset _neph_keychain

  # Show mdstat
  [[ ! -f /proc/mdstat ]] || cat /proc/mdstat
fi
