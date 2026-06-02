#!/bin/bash

set -euo pipefail
. ~/bin/lib/util.sh

OWD="$(cd -- "$(dirname -- "$0")" && pwd)"

unset ctr
cleanup() {
  [[ -z ${ctr-} ]] || cmd buildah rm "$ctr"
}
trap cleanup EXIT

USER=nephyrin
PASS=dumbpass
AUR_PACKAGES=(paru claude-code)

## Blank container
ctr=$(cmd buildah from scratch)

## Use buildah unshare+mount to pacstrap 'base' from the host
pacstrapify() {
  set -euo pipefail
  m=$(buildah mount "$ctr")
  local db="$m"/var/lib/pacman
  local cache="$m"/var/cache/pacman/pkg
  mkdir -pv -- "$db" "$cache"
  pacman --noconfirm --cachedir "$cache" --root "$m" --dbpath "$db" --disable-sandbox -Sy base
  cp -v -- /etc/pacman.conf "$m"/etc
  cp -v -- /etc/pacman.d/mirrorlist "$m"/etc/pacman.d/mirrorlist
}

cmd buildah unshare env ctr="$ctr" bash -c "function $(declare -f pacstrapify); pacstrapify"

# buildah_run_func <container> <function_name> [args]
#  Run stuff in the container like a bad Containerfile
#
#  Runs bash in the container, passing given function to it and calling it.  Sets `-euo pipefail`.
#  Function needs to be standalone, we just `declare -f funcname` to export it to the subshell.
buildah_run_func() {
  local ctr="$1"
  local funcname="$2"
  local args=("${@:3}")
  # Cobble together a bash script that re-declares said function and then invokes it with '$@', which we pass in as args
  local script
  script="set -euo pipefail; $(declare -f -- "$funcname") && ${funcname@Q} \"\$@\""
  buildah run -- "$ctr" bash -c "$script" -- "${args[@]}"
}


# - Cleanup pacman.conf
# - Set mirror
# - Setup keyring
# - Update
# - adopt all pacnews
# - cleanup pacman.conf again (it might've gotten pacnew'd)
# - Reinstall packages missing files (from deleting NoExtract lines)
# - Add $USER and build users
# - Set $USER pass
# - Enable passwordless sudo (for now)
setup_root() {
  local USER="$1"
  local PASS="$2"
  sed -ri '/^NoExtract / d' /etc/pacman.conf
  sed -ri 's|^#?(ParallelDownloads) .*|\1 = 20|' /etc/pacman.conf
  echo "Server = http://mirror.pointysoftware.net/archlinux/\$repo/os/\$arch" > /etc/pacman.d/mirrorlist
  pacman-key --init
  pacman-key --populate
  pacman --disable-sandbox -S --noconfirm archlinux-keyring
  pacman --disable-sandbox --noconfirm -Suu
  # shellcheck disable=SC2046
  pacman --disable-sandbox -S --noconfirm $(pacman -Qnq)
  pacman --disable-sandbox -S --noconfirm --needed git sudo base base-devel
  rm -vf /etc/pacman.d/mirrorlist.pacnew /etc/{passwd,gpasswd,group,shadow,gshadow}.pacnew
  find / -xdev -name '*.pacnew' -exec bash -c 'mv -v -- "$1" "${1%.pacnew}"' -- {} \;
  sed -ri 's|^#Color|Color|' /etc/pacman.conf
  sed -ri 's|^#?(ParallelDownloads) .*|\1 = 20|' /etc/pacman.conf
  sed -ri 's|^#(NoProgressBar)|#\1|' /etc/pacman.conf
  sed -ri 's|^#(DisableSandbox)|\1|' /etc/pacman.conf
  useradd "$USER" -m -G wheel
  printf %s "$USER:$PASS" | chpasswd
  # Add throwaway build user second so real user gets uid 1000
  useradd build -m -G wheel
  # Make sudo NOPASSWD just for build step, setup_root_final switches back
  echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' > /etc/sudoers.d/wheel
  # Setup machineid for build step, final step clears it
  systemd-machine-id-setup
}

cmd buildah_run_func "$ctr" setup_root "$USER" "$PASS"

# Switch to build user, build and install desired AUR stuff
setup_user() {
  for aurpkg in "$@"; do
    cd
    git clone https://aur.archlinux.org/"$aurpkg".git
    cd -- "$aurpkg"
    makepkg --noconfirm -scri
  done
}

cmd buildah config --user build "$ctr"
cmd buildah_run_func "$ctr" setup_user "${AUR_PACKAGES[@]}"

# Switch back to root
# - nuke build user
# - change back to passwordful sudo
# - nuke cache/temp files
# - init file sudo
setup_root_final() {
  set -euo pipefail
  echo '%wheel ALL=(ALL:ALL) ALL' > /etc/sudoers.d/wheel
  userdel build
  rm -rf /home/build /var/cache/pacman/pkg /etc/pacman.d/gnupg /etc/machine-id /var/lib/dbus/machine-id
  echo 'ALL    ALL=(root) NOPASSWD: /init.sh' > /etc/sudoers.d/zz-entrypoint
  echo 'Defaults!/init.sh env_keep += "NEPHARCH_INIT_PACKAGES"' >> /etc/sudoers.d/zz-entrypoint
  echo 'Defaults!/init.sh env_keep += "NEPHARCH_PASSWORDLESS_SUDO"' >> /etc/sudoers.d/zz-entrypoint
}

cmd buildah config --user root "$ctr"
cmd buildah_run_func "$ctr" setup_root_final

# Throw our init script in
cmd buildah copy --chmod=755 -- "$ctr" "$OWD"/nepharch.init.sh /init.sh
cmd buildah config --entrypoint '["/init.sh"]' "$ctr"

# Final config -- default to real user
cmd buildah config --user "$USER" "$ctr"
cmd buildah config --workingdir /home/"$USER" "$ctr"

tag="$(date +%s)"
cmd buildah commit --squash -- "$ctr" "nepharch:$tag"
cmd buildah tag "nepharch:$tag" nepharch:latest

estat "Succesfully built nepharch:$tag"
