#!/usr/bin/bash -eu


pacman -S --needed --noconfirm reflector
reflector --age 12 --latest 8 --sort rate --country China -p https \
  --save /etc/pacman.d/mirrorlist \
  --save pacman.d/mirrorlist \
  -x neusoft -x qlu

# install -m644 {,/etc/}locale.gen
# install -m644 {,/etc/}locale.conf
# install -m644 {,/etc/}pacman.conf
# locale-gen
