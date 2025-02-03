#!/usr/bin/env sh
set -eux
cd scripts

# curl -fLOJSs https://raw.githubusercontent.com/mpv-player/mpv/master/TOOLS/lua/autoload.lua

# curl -fLOJSs --output-dir ../script-modules https://github.com/Seme4eg/mpv-scripts/raw/master/script-modules/extended-menu.lua

# parallel -j 1 curl -fLOJ -- \

curl -fLOJSs https://github.com/Seme4eg/mpv-scripts/raw/master/M-x.lua
curl -fLOJSs https://github.com/Eisa01/mpv-scripts/raw/master/scripts/SimpleHistory.lua
curl -fLOJSs https://github.com/Eisa01/mpv-scripts/raw/master/scripts/SmartCopyPaste.lua
curl -fLOJSs https://github.com/Eisa01/mpv-scripts/raw/master/scripts/UndoRedo.lua
curl -fLOJSs https://github.com/tsl0922/mpv-menu-plugin/raw/main/src/lua/dialog.lua
curl -fLOJSs https://github.com/tsl0922/mpv-menu-plugin/raw/main/src/lua/dyn_menu.lua
curl -fLOJSs https://github.com/occivink/mpv-scripts/raw/master/scripts/seek-to.lua
curl -fLOJSs https://github.com/fbriere/mpv-scripts/raw/master/scripts/sub-fonts-dir-auto.lua
curl -fLOJSs https://github.com/po5/thumbfast/raw/master/thumbfast.lua
curl -fLOJSs https://github.com/dyphire/mpv-config/raw/master/scripts/auto-save-state.lua

# curl -fLOJSs --output-dir bilibiliAssert https://github.com/itKelis/MPV-Play-BiliBili-Comments/raw/main/scripts/bilibiliAssert/Danmu2Ass.py
# curl -fLOJSs --output-dir bilibiliAssert https://github.com/itKelis/MPV-Play-BiliBili-Comments/raw/main/scripts/bilibiliAssert/main.lua

# clipshot.lua
# sub_fonts_dir_auto
# sub-export
# chapterskip.lua
