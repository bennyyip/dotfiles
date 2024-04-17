$EDITOR = "vim"
$XONSH_HISTORY_MATCH_ANYWHERE = True
$XONSH_HISTORY_SIZE = (2**20, 'commands')
$XONSH_HISTORY_BACKEND = 'sqlite'
$HISTCONTROL = {'erasedups', 'ignorespace', 'ignoreerr'}

$XONSH_AUTOPAIR= True

if platform.ON_WINDOWS:
    $XONTRIB_SH_SHELLS = ['pwsh']
    $FORCE_POSIX_PATHS = True
    $LANG = 'en_US.UTF-8'
    aliases['tldr'] = 'tealdeer-windows-x86_64-msvc'


user_bins = [
    f'{$HOME}/bin',
]

for dir in user_bins:
    if path.isdir(dir) and path.exists(dir):
        $PATH.add(dir,front=True, replace=True)
