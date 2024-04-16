#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True

from pathlib import Path
from xonsh import platform


if platform.ON_WINDOWS:
    CONFIG="windows.yaml"
else:
    CONFIG="linux.yaml"

DOTBOT_DIR="dotbot"
DOTBOT_BIN="bin/dotbot"

BASEDIR = Path(__file__).parent.absolute()

cd @(BASEDIR)

git -C @(DOTBOT_DIR) submodule sync --quiet --recursive
git submodule update --init --recursive @(DOTBOT_DIR)

if platform.ON_WINDOWS:
    python @(BASEDIR/DOTBOT_DIR/DOTBOT_BIN) -d @(BASEDIR) -c @(CONFIG) @($ARGS[1:])
else:
    @(BASEDIR/DOTBOT_DIR/DOTBOT_BIN) -d @(BASEDIR) -c @(CONFIG) @($ARGS[1:])
