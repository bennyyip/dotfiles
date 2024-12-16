#!/usr/bin/bash -eu

install -m644 {,/etc/}locale.gen
install -m644 {,/etc/}locale.conf
install -m644 {,/etc/}pacman.conf
locale-gen

pacman -S --needed --noconfirm reflector
reflector --latest 8 --sort age --save /etc/pacman.d/mirrorlist --country China
systemctl enable reflector.timer --now
