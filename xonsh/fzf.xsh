import os
import re
import subprocess
from xonsh.history.main import history_main
from xonsh.completers.path import complete_path
from prompt_toolkit.keys import Keys

$FZF_DEFAULT_OPTS = '--bind tab:down,shift-tab:up --layout=reverse'
$FZF_DEFAULT_COMMAND = 'fd --type f --strip-cwd-prefix --hidden --follow --exclude .git --exclude .vscode --exclude __pycache__'
$FZF_CTRL_T_COMMAND = $FZF_DEFAULT_COMMAND

def get_fzf_binary_name():
    fzf_tmux_cmd = 'fzf-tmux'
    if 'TMUX' in ${...} and $(which fzf_tmux_cmd):
        return fzf_tmux_cmd
    return 'fzf'

def get_fzf_binary_path():
    path = $(which @(get_fzf_binary_name()))
    if not path:
        raise Exception("Could not determine path of fzf using `which`; maybe it is not installed or not on PATH?")
    return path


fzf_cmd = get_fzf_binary_name()

@aliases.register('vff')
@unthreadable
def __vff():
    f = $(@(fzf_cmd) --prompt "gvim> " ).strip()
    if f != '':
        vr @(f)

@aliases.register('vfr')
@unthreadable
def __vfr():
    if platform.ON_WINDOWS:
        mru_path = pf'{$APPDATA}/LeaderF/python3/mru/mruCache'
    else:
        mru_path = pf'{$HOME}/.LfCache/python3/mru/mruCache'
        if not mru_path.exists():
            mru_path = pf'{$HOME}/.LfCache/python2/mru/mruCache'

    f = $(cat @(mru_path) | @(fzf_cmd) --prompt "gvim> " ).strip()
    if f != '':
        vr @(f)

@aliases.register('scd')
@unthreadable
def __scd(args):
    p = $(fd -E .git -E __pycache__ -E .vscode -H -t d @(args) | @(fzf_cmd) --prompt "cd> ").strip()
    if p != '':
        cd @(p)


@aliases.register('glook')
@unthreadable
def __glook():
    cd @$(ghq list -p | @(fzf_cmd))

@events.on_ptk_create
def custom_keybindings(bindings, **kw):
    @bindings.add('escape', 's')
    def __foo(event):
        choice = $(history show all -r | @(fzf_cmd) '--prompt' 'cmd> ' '--no-sort').strip()

        if choice:
            event.current_buffer.text = choice
            event.current_buffer.cursor_position = len(choice)
