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
    # Container started as non-root, run init as root.
    #
    # All users can sudo-call this. We are a no-op once we've run once.
    sudo /init.sh
  fi
fi

# Don't chain when called via sudo though
[[ -n ${SUDO_UID-} ]] && exit 0

##
## Did init steps, now exec normal command or drop user if NEPHARCH_USER passed
##

[[ $# -gt 0 ]] || set -- /bin/bash
unset NEPHARCH_INIT_PACKAGES
unset NEPHARCH_PASSWORDLESS_SUDO

# If a username is passed here on spawn, dynamically change uid 1000 username and home path to this and then drop to
# said user.  e.g. `--env=NEPHARCH_USER=myusername -u 0`.  Alternatively do `-u 1000` and use the default built into the
# container.
#
# Also useful for buggy krun user handling.  When spawned directly as root, but with this set, we'll drop privileges to
# the desired user to run the command.
#
#   https://github.com/libkrun/libkrun/issues/123
if [[ -n ${NEPHARCH_USER-} ]]; then
  default_user=$(id -n -u 1000)
  #FIXME This breaks if you've mounted things as children of it on container start...
  if [[ $default_user != "$NEPHARCH_USER" ]]; then
    usermod -m -l "$NEPHARCH_USER" -d /home/"$NEPHARCH_USER" "$default_user"
  fi
  exec env -u NEPHARCH_USER sudo -u "${NEPHARCH_USER-}" -- "$@"
fi

# Otherwise just run the command
exec "$@"
