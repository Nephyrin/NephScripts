#!/bin/zsh

###
### Init, load local config file, neph stuff, stuff shared with bash
###

# Profiling: Uncomment lines at bottom too
# zmodload zsh/zprof

# This seems to just make the prompt slower at startup? May be a bad interaction with gitstatusd.
typeset -g POWERLEVEL9K_INSTANT_PROMPT=off

# Path to nephscripts and private
NEPH=~/neph
NPRIV=~/neph/priv

# aliases.sh and such expects all shell types to provide this
_neph_addtopath() {
  local new
  for new in "$@"; do
    [[ ! -d $new || ${path[(ie)$new]} -le ${#path[@]} ]] || path+=($new)
  done
}
_neph_addtopath $NEPH/bin
_neph_addtopath $NPRIV/bin

# Add things here in chain-loaded files to execute them after p10k
_neph_zsh_post_init_funcs=()
_neph_zsh_post_init() { _neph_zsh_post_init_funcs+=($@); }


# Shared interactive shell startup.  Do before initializing instant-prompt. (so, this part not covered by instant-ness)
[[ ! -f $NEPH/aliases.sh ]]                || source $NEPH/aliases.sh
[[ ! -f $NEPH/interactive-shell-init.sh ]] || source $NEPH/interactive-shell-init.sh
[[ ! -f $NPRIV/zshrc ]]                    || source $NPRIV/zshrc
[[ ! -f ~/.zshrc.local ]]                  || source ~/.zshrc.local

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

###
### oh-my-zsh
###

# Path to your oh-my-zsh installation.
export ZSH=~/.zsh.d/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.zsh.d/oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.

ZSH_THEME="powerlevel10k/powerlevel10k"
# ZSH_THEME="terminalparty"
# ZSH_THEME="wezm"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# p10k "helpfully" wgets binaries for gitstatus from the internet if it wants to.  Thanks guys, real high quality.
GITSTATUS_AUTO_INSTALL=0

# Just grab this from AUR if you want it
GITSTATUS_DAEMON=/usr/share/gitstatus/usrbin/gitstatusd
[[ -e $GITSTATUS_DAEMON ]] || _POWERLEVEL9K_DISABLE_GITSTATUS=1

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
export ZSH_CUSTOM=~/.zsh.d/

# Which plugins would you like to load? (plugins can be found in ~/.zsh.d/oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.zsh.d/oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git compleat zsh-syntax-highlighting zsh-autosuggestions)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

###
### P10K
###

# use ctrl+t to toggle autosuggestions(hopefully this wont be needed as
# zsh-autosuggestions is designed to be unobtrusive)
# (conflicts with fzf)
# bindkey '^T' autosuggest-toggle

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Stop writing history for this session
prompt_nohist() {
  [[ -z ${_nohist-} ]] || p10k segment -f 208 -i '⭐' -t 'nohist'
}

prompt_promptnote() {
  [[ -z ${_NEPH_PN-} ]] || p10k segment -f 208 -i '⭐' -t $_NEPH_PN
}

# use ctrl+t to toggle autosuggestions(hopefully this wont be needed as
# zsh-autosuggestions is designed to be unobtrusive)
# (conflicts with fzf)
# bindkey '^T' autosuggest-toggle
zshaddhistory() { local args=(${=1}); [[ -z ${_nohist-} && "${args[1]}" != nohist ]] || return 1; }

_p10k_reload() { ! whence p10k &>/dev/null || p10k reload; }

nohist()
{
  typeset -g _nohist=1
  _p10k_reload
}

promptnote() { typeset -g _NEPH_PN="$1" && _p10k_reload; }

zshaddhistory() { local args=(${=1}); [[ "${args[1]}" != nohist ]] || return 1; }

nohist()
{
  local file=${1:+/tmp/zsh.$1}
  typeset -g _nohist=1
  HISTFILE=${file:-$(mktemp --tmpdir zsh.XXX)}
  _p10k_reload
}


###
### fzf binds if installed
###

[[ ! -f /usr/share/fzf/key-bindings.zsh ]] || source /usr/share/fzf/key-bindings.zsh

###
### Custom ZSH binds and aliases
###

bindkey "^O" accept-and-hold
bindkey "^N" accept-and-infer-next-history

###
### Atuin
###

eval "$(atuin init zsh --disable-up-arrow || true)"
bindkey ^q atuin-up-search

###
### Post-init stuff from loaded files
###

for func in ${_neph_zsh_post_init_funcs[@]}; do
  $func
done

# Profiling: Uncomment line at top too
# zprof > ~/.cache/neph-zprof
# zmodload -u zsh/zprof
