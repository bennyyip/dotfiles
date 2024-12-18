#!/bin/sh -xue
git upgrade-submodules fast-syntax-highlighting
wget https://github.com/zsh-users/zsh-autosuggestions/raw/master/zsh-autosuggestions.zsh -O zsh-autosuggestions.zsh
wget https://github.com/hlissner/zsh-autopair/raw/master/autopair.zsh -O autopair.zsh
wget https://github.com/shyiko/commacd/raw/master/commacd.sh -O $HOME/dotfiles/shell/plugins/commacd.sh
wget https://github.com/ohmyzsh/ohmyzsh/raw/refs/heads/master/plugins/git/git.plugin.zsh -O git.zsh
