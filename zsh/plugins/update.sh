#!/bin/sh -xue
# git upgrade-submodules fast-syntax-highlighting
wget https://github.com/hlissner/zsh-autopair/raw/master/autopair.zsh -O autopair.zsh
wget https://raw.githubusercontent.com/paulirish/git-open/refs/heads/master/git-open -O ~/dotfiles/bin/git-open
chmod +x ~/dotfiles/bin/git-open
