from xonsh.platform import ON_WINDOWS
import os

$MY_PROXY = "http://127.0.0.1:10809"

$AUTO_CD = True

# workaround to get C-x C-e works on windows
# https://github.com/xonsh/xonsh/issues/4619
os.environ["EDITOR"] = "vim"
$EDITOR = "vim"
$XONSH_HISTORY_MATCH_ANYWHERE = True
$XONSH_HISTORY_SIZE = (2**20, 'commands')
$XONSH_HISTORY_BACKEND = 'sqlite'
$HISTCONTROL = {'erasedups', 'ignorespace', 'ignoreerr'}

$XONSH_AUTOPAIR = True

# https://wiki.archlinux.org/title/Color_output_in_console#Using_less
$INTENSIFY_COLORS_ON_WIN = False
$MANPAGER="less -R --use-color -Dd+r -Du+b"
$MANROFFOPT="-P -c"
$LESS="-FRXM"
# Coloured man page support
# using 'less' env vars (format is '\E[<brightness>;<colour>m')
$LESS_TERMCAP_mb = "\033[01;31m"     # begin blinking
$LESS_TERMCAP_md = "\033[01;31m"     # begin bold
$LESS_TERMCAP_me = "\033[0m"         # end mode
$LESS_TERMCAP_so = "\033[01;44;36m"  # begin standout-mode (bottom of screen)
$LESS_TERMCAP_se = "\033[0m"         # end standout-mode
$LESS_TERMCAP_us = "\033[00;36m"     # begin underline
$LESS_TERMCAP_ue = "\033[0m"         # end underline

# $LS_COLORS = $(cat ~/.shell/LS_COLORS)

$RIPGREP_CONFIG_PATH = f'{$HOME}/.ripgreprc'

if ON_WINDOWS:
    $XONTRIB_SH_SHELLS = ['pwsh']
    $FORCE_POSIX_PATHS = True
    $AGV_EDITOR='gvim --remote-silent-tab'
    $LANG = 'en_US.UTF-8'
    aliases['tldr'] = 'tealdeer-windows-x86_64-msvc'
    $null = "NUL"
else:
    $null = "/dev/null"


user_bins = [
    f'{$HOME}/bin',
]

for dir in user_bins:
    if path.isdir(dir) and path.exists(dir):
        $PATH.add(dir,front=True, replace=True)
