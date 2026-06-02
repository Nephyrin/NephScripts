#!/bin/bash
# Entrypoint for nepharch container, just does first-time setup once
set -euo pipefail
if [[ ! -f /.nepharch-init ]]; then
  if [[ ${EUID-} -eq 0 ]]; then
    (
      set -e
      echo ":: Running first time init"
      pacman-key --init
      pacman-key --populate
      systemd-machine-id-setup
      if [[ -n ${NEPHARCH_PASSWORDLESS_SUDO-} ]]; then
        echo ":: Enabling passwordless-sudo"
        echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' > /etc/sudoers.d/wheel
      fi
      if [[ -n ${NEPHARCH_INIT_PACKAGES-} ]]; then
        echo ":: Installing $NEPHARCH_INIT_PACKAGES"
        set -f
        # shellcheck disable=SC2086 # we want splitting, set -f prevents globbing
        pacman -Syu --noconfirm -- $NEPHARCH_INIT_PACKAGES
      fi
    ) >&2
    touch /.nepharch-init
  else
    # All users can sudo-call this. We are a no-op once we've run once.
    sudo /init.sh
  fi
fi

# Don't chain when called via sudo though
[[ -n ${SUDO_UID-} ]] && exit 0

[[ $# -gt 0 ]] || set -- /bin/bash
unset NEPHARCH_INIT_PACKAGES
unset NEPHARCH_PASSWORDLESS_SUDO
exec "$@"
