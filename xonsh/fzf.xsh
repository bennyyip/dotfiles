import os
import re
import subprocess
from xonsh.history.main import history_main
from xonsh import platform
from xonsh.completers.path import complete_path
from prompt_toolkit.keys import Keys

$FZF_DEFAULT_OPTS = '--bind tab:down,shift-tab:up --layout=reverse'
$FZF_DEFAULT_COMMAND = 'fd --type f --strip-cwd-prefix --hidden --follow --exclude .git --exclude .vscode --exclude __pycache__'
$FZF_CTRL_T_COMMAND = $FZF_DEFAULT_COMMAND
$FZF_DEFAULT_OPTS=f"{$FZF_DEFAULT_OPTS} --color=bg+:#3c3836,bg:#282828,spinner:#fb4934,hl:#928374,fg:#ebdbb2,header:#928374,info:#8ec07c,pointer:#fb4934,marker:#fb4934,fg+:#ebdbb2,prompt:#fb4934,hl+:#fb4934"

$_ZO_FZF_OPTS = $FZF_DEFAULT_OPTS

def get_fzf_binary_name():
    fzf_tmux_cmd = 'fzf-tmux'
    if 'TMUX' in ${...} and $(which @(fzf_tmux_cmd)):
        return fzf_tmux_cmd
    return 'fzf'

def get_fzf_binary_path():
    path = $(which @(get_fzf_binary_name()))
    if not path:
        raise Exception("Could not determine path of fzf using `which`; maybe it is not installed or not on PATH?")
    return path


fzf_cmd = get_fzf_binary_name()

@aliases.register('vff')
def __vff():
    f = $(@(fzf_cmd) --prompt "gvim> " ).strip()
    if f != '':
        vr @(f)

@aliases.register('vfr')
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
def __scd(args):
    p = $(fd -E .git -E __pycache__ -E .vscode -H -t d @(args) | @(fzf_cmd) --prompt "cd> ").strip()
    if p != '':
        cd @(p)


@aliases.register('glook')
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
