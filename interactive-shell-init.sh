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
  ! type rg &>/dev/null || export FZF_DEFAULT_COMMAND="fd --hidden"
  ! type fd &>/dev/null || export FZF_CTRL_T_COMMAND="fd --hidden"
  ! type fd &>/dev/null || export FZF_ALT_C_COMMAND="fd --hidden -E .git -t d"

  # Generic function to generate a preview
  _neph_fzf_preview() {
    set -o pipefail
    local file=$1
    if [[ -d $file ]]; then
      # If directory contains a PKGBUILD, pull out a few bits
      grep 2>/dev/null -P '^(pkgbase|pkgname|pkgrel)=' -- "$file"/PKGBUILD | bat -pP -lsh --color=always && echo
      # Show a few lines of history for dir
      git ll --color=always -n 5 -- "$file" 2>/dev/null
      echo
      if command -v eza &>/dev/null; then
        eza -F --color-scale=age --icons --smart-group --git --time-style=relative -l --sort=modified --color=always \
            -- "$file"
      else
        ls -ltr --color=always -- "$file"
      fi
    elif command -v bat &>/dev/null; then
      bat -pP -- "$file"
    else
      local enc
      enc=$(file -b --mime-encoding 2>/dev/null) ||:
      if [[ -n $enc && $enc != binary ]]; then
        cat -- "$file"
      else
        echo "Cannot preview file without \`file\` or \`bat\` to avoid spewing binary"
      fi
    fi
  }

  ## TODO abusing the tab-trick to make this stuff searchable would be neat
  # fzf does dumb quote parsing out of this, not a full eval -- just escaping \ and " seems to work
  _neph_fzf_preview_sh="$(typeset -f _neph_fzf_preview); _neph_fzf_preview {}"
  _neph_fzf_preview_sh=${_neph_fzf_preview_sh//\\/\\\\}
  _neph_fzf_preview_sh=${_neph_fzf_preview_sh//\"/\\\"}
  export FZF_ALT_C_OPTS="--preview \"${_neph_fzf_preview_sh}\""
  unset _neph_fzf_preview_sh

  # Enable a floating pane in tmux mode
  # export FZF_TMUX_OPTS='-p90%,40% -x 0% -y 100%'

  unset _inc_type
fi
