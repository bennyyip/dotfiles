#!/bin/sh -xue
git upgrade-submodules fast-syntax-highlighting &
git upgrade-submodules git-fuzzy &
wget https://github.com/zsh-users/zsh-autosuggestions/raw/master/zsh-autosuggestions.zsh -O zsh-autosuggestions.zsh &
wget https://github.com/skywind3000/z.lua/raw/master/z.lua -O z.lua &
wget https://github.com/skywind3000/z.lua/raw/master/z.lua.plugin.zsh -O z.lua.plugin.zsh &
wget https://github.com/hlissner/zsh-autopair/raw/master/autopair.zsh -O autopair.zsh &
wget https://github.com/shyiko/commacd/raw/master/commacd.sh -O $HOME/dotfiles/shell/plugins/commacd.sh &
wait
