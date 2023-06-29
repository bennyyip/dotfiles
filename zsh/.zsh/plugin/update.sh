#!/bin/sh -xue
git upgrade-submodules fast-syntax-highlighting
wget https://github.com/zsh-users/zsh-autosuggestions/raw/master/zsh-autosuggestions.zsh -O zsh-autosuggestions.zsh
wget https://github.com/skywind3000/z.lua/raw/master/z.lua -O z.lua
wget https://github.com/skywind3000/z.lua/raw/master/z.lua.plugin.zsh -O z.lua.plugin.zsh
