## About

These are misc scripts and configuration files I use on my machine. Not
guaranteed to be portable but go nuts if you find anything useful. Copyright is
not always well documented and I don't guarantee that any code without explicit
copyright is not somehow copyright or patent encumbered.

Feel free to contact me if you want clearer copyright information on any
particular bit.

Most ~/bin/ stuff *should* be path agnostic, but the bashrc assumes ~/bin
exists. Various mozilla-related scripts assume ~/moz/ and ~/moz/moz-git/

## Submodules

A bunch of the zsh/emacs stuff has submodules for specific versions of
tools/plugins/etc.  After cloning/pulling one should run:

    git submodule update --init --recursive

A lot of said submodules use github ssh URLs. If you do not have a
github-enabled ssh key you could jam this in your git config:

    [url "https://github.com/"]
    insteadOf = git@github.com:

## Setup / symlinks for various things:

_I'm probably not keeping this list up to date with everything in here_

        # Scripts/bash
        ln -sv $repo/bin            ~/bin
        ln -sv $repo/bashrc         ~/.bashrc
        ln -sv $repo/bash_profile   ~/.bash_profile

        # zsh
        ln -sv "$repo"/zshrc ~/.zshrc
        ln -sv "$repo"/zsh.d ~/.zsh.d

        # ack
        ln -sv "$repo"/ackrc ~/.ackrc

        # Emacs (Make sure you submodule init && update)
        ln -sv $repo/emacs          ~/.emacs
        ln -sv $repo/emacs.d        ~/.emacs.d
        # Byte compile things and setup CEDET
        ( cd ~/.emacs.d && ./compile.sh )

        # Git
        ln -sv $repo/gitconfig      ~/.gitconfig
        ln -sv $repo/gitignore      ~/.gitignore

        # GDB
        ln -sv $repo/gdbinit ~/.gdbinit

        # Mercurial
        ln -sv $repo/hgrc           ~/.hgrc
        ln -sv $repo/hgignore       ~/.hgignore

        # Powerline (needs the PYTHONPATH and PATH additions in bashrc)
        ( cd "$repo" && ./build-powerline.sh )
        mkdir -v ~/.fonts.conf.d/
        mkdir -v ~/.fonts/
        ln -sv $repo/powerline.git/font/10-powerline-symbols.conf ~/.fonts.conf.d/
        ln -sv $repo/powerline.git/font/PowerlineSymbols.otf ~/.fonts/
        fc-cache -vf

        # ssh config (does not want this file symlinked/insecure)
        cp -v $repo/sshconfig ~/.ssh/config
        chown -v $USER ~/.ssh/config
        chmod -v 600 ~/.ssh/config

        # ls++ config
        mkdir -pv ~/ls++
        ln -sv $repo/ls++.conf ~/ls++/

        # atuin
        ln -sv $repo/atuin ~/.config/

## Contact
- john@pointysoftware.net
