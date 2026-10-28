**Minimal nginx / rootless Podman**

    buildah build neph-nginx -f Containerfile

    cp -v neph-nginx.service ~/.config/systemd/user/

    # Optional, helper to expose /run/user/<user>/port{80,443}.socket for nginx.conf use
    #   (edit nginx.conf to listen to said unix socket)
    sudo cp -v neph-port* /etc/systemd/system/
    sudo systemctl enable --now neph-port-80-to-user@johns.service
    sudo systemctl enable --now neph-port-443-to-user@johns.service

    # Make a data dir
    mkdir -pv ~/web/{data,www,certs}
    cp ./nginx.conf ~/web  # edit as needed
    systemctl --user enable --now neph-nginx@"$HOME/web"

To run at boot and remain running after logout (optional):

    sudo loginctl enable-linger "$USER"
