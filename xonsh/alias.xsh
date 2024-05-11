import os
import shutil
from pathlib import Path

import requests

from xonsh.platform import ON_WINDOWS, ON_LINUX
from xonsh.cli_utils import Annotated, Arg, ArgParserAlias

# fmt:off
if ON_LINUX:
    aliases |= {
        "Fo": ["pacman", "-F"],
        "Fy": ["sudo", "pacman", "-Fy"],
        "Qi": ["pacman", "-Qi"],
        "Ql": ["pacman", "-Ql"],
        "Qo": ["pacman", "-Qo"],
        "Qs": ["pacman", "-Qs"],
        "Rcs": ["sudo", "pacman", "-Rcs"],
        "Si": ["pacman", "-Si"],
        "Ss": ["pacman", "-Ss"],
        "pain": ["sudo", "pacman", "-S", "--needed"],
        "painn": ["sudo", "pacman", "-S"],
    }

    @aliases.register("Syu")
    def __Syu(args):
        sudo pacman -Syu
        pacman -Qtdq | ifne sudo pacman -Rcs -

aliases |= {
    "scp-resume": ["rsync", "--partial", "-h", "--progress", "--rsh=ssh"],
    "ipynb": ["jupyter", "notebook", "--no-browser"],
    "grep": ["grep", "--color=auto"],
    "egrep": ["egrep", "--color=auto"],
    "fgrep": ["fgrep", "--color=auto"],
    "l": "ls -lah --color=auto",
    "ls": ["ls", "--color=auto", "-v"],
    "vr": ["gvim", "--remote-silent-tab"],
    "gget": ["ghq", "get", "--no-recursive", "--shallow"],
    "update": ["source", "~/.xonshrc"],
    "7tar": ["7z", "a", "-mmt"],
    "7z": ["7z", "-xr!*~", "-xr!*.swp"],
    ":q": ["exit"],
    ":qa": ["tmux", "detach"],
    "Fo": ["pacman", "-F"],
    "Fy": ["sudo", "pacman", "-Fy"],
    "Qi": ["pacman", "-Qi"],
    "Ql": ["pacman", "-Ql"],
    "Qo": ["pacman", "-Qo"],
    "Qs": ["pacman", "-Qs"],
    "Rcs": ["sudo", "pacman", "-Rcs"],
    "Si": ["pacman", "-Si"],
    "Ss": ["pacman", "-Ss"],
    "Ssa": ["pacaur", "-Ssa"],
    "bc": ["bc", "-l"],
    "bpy": ["bpython"],
    "btar": ["tar", "-Ilbzip3", "cjfv"],
    "clip": ["xsel", "-i", "-b"],
    "cower": ["cower", "--domain", "aur.tuna.tsinghua.edu.cn"],
    "cp": ["cp", "-i", "--reflink=auto"],
    "diff-so-fancy": ["diff-so-fancy", "|", "less"],
    "e": ["emacsclient", "-nw"],
    "ec": ["emacsclient", "-c", "-n"],
    "er": ["emacsclient", "-n"],
    "g": ["git"],
    "ga": ["git", "add"],
    "gaa": ["git", "add", "--all"],
    "gapa": ["git", "add", "--patch"],
    "gau": ["git", "add", "--update"],
    "gb": ["git", "branch"],
    "gba": ["git", "branch", "-a"],
    "gbd": ["git", "branch", "-d"],
    "gbda": ["git", "branch", "--no-color", "--merged", "|", "command", "grep", "-vE", "^(\\*|\\s*(master|develop|dev)\\s*$)", "|", "command", "xargs", "-n", "1", "git", "branch", "-d"],
    "gbl": ["git", "blame", "-b", "-w"],
    "gbnm": ["git", "branch", "--no-merged"],
    "gbr": ["git", "branch", "--remote"],
    "gbs": ["git", "bisect"],
    "gbsb": ["git", "bisect", "bad"],
    "gbsg": ["git", "bisect", "good"],
    "gbsr": ["git", "bisect", "reset"],
    "gbss": ["git", "bisect", "start"],
    "gc": ["git", "commit", "-v"],
    "gc!": ["git", "commit", "-v", "--amend"],
    "gca": ["git", "commit", "-v", "-a"],
    "gca!": ["git", "commit", "-v", "-a", "--amend"],
    "gcam": ["git", "commit", "-a", "-m"],
    "gcaN": ["git", "commit", "-v", "-a", "--no-edit", "--amend"],
    "gcans!": ["git", "commit", "-v", "-a", "-s", "--no-edit", "--amend"],
    "gcb": ["git", "checkout", "-b"],
    "gcf": ["git", "config", "--list"],
    "gcl": ["git", "clone", "--recursive"],
    "gclean": ["git", "clean", "-fd"],
    "gcm": ["git", "commit", "-m"],
    "gcn!": ["git", "commit", "-v", "--no-edit", "--amend"],
    "gco": ["git", "checkout"],
    "gcount": ["git", "shortlog", "-sn"],
    "gcp": ["git", "cherry-pick"],
    "gcpa": ["git", "cherry-pick", "--abort"],
    "gcpc": ["git", "cherry-pick", "--continue"],
    "gcs": ["git", "commit", "-S"],
    "gcsm": ["git", "commit", "-s", "-m"],
    "gd": ["git", "diff"],
    "gdca": ["git", "diff", "--cached"],
    "gdt": ["git", "diff-tree", "--no-commit-id", "--name-only", "-r"],
    "gdw": ["git", "diff", "--word-diff"],
    "ggc": ["git", "reflog", "expire", "--all", "--expire=now", "&&", "git", "gc", "--aggressive", "--quiet", "--prune=now"],
    "gl": ["git", "pull"],
    "glg": ["git", "log", "--graph", "--pretty=format:%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %Cblue<%an>%Creset", "--abbrev-commit", "--date=relative", "--all"],
    "glgg": ["git", "log", "--graph"],
    "glgga": ["git", "log", "--graph", "--decorate", "--all"],
    "glgm": ["git", "log", "--graph", "--max-count=10"],
    "glgp": ["git", "log", "--stat", "-p"],
    "glo": ["git", "log", "--oneline", "--decorate"],
    "glog": ["git", "log", "--oneline", "--decorate", "--graph"],
    "gloga": ["git", "log", "--oneline", "--decorate", "--graph", "--all"],
    "glol": ["git", "log", "--graph", "--pretty=%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset", "--abbrev-commit"],
    "glola": ["git", "log", "--graph", "--pretty=%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset", "--abbrev-commit", "--all"],
    "glp": ["_git_log_prettily"],
    "glum": ["git", "pull", "upstream", "master"],
    "gm": ["git", "merge"],
    "gmom": ["git", "merge", "origin/master"],
    "gmt": ["git", "mergetool", "--no-prompt"],
    "gmtvim": ["git", "mergetool", "--no-prompt", "--tool=vimdiff"],
    "gmum": ["git", "merge", "upstream/master"],
    "gp": ["git", "push"],
    "gpd": ["git", "push", "--dry-run"],
    "gpoat": ["git", "push", "origin", "--all", "&&", "git", "push", "origin", "--tags"],
    "gpristine": ["git", "reset", "--hard", "&&", "git", "clean", "-dfx"],
    "gpu": ["git", "push", "upstream"],
    "gpv": ["git", "push", "-v"],
    "gr": ["git", "remote"],
    "gra": ["git", "remote", "add"],
    "grb": ["git", "rebase"],
    "grba": ["git", "rebase", "--abort"],
    "grbc": ["git", "rebase", "--continue"],
    "grbi": ["git", "rebase", "-i"],
    "grbm": ["git", "rebase", "master"],
    "grbs": ["git", "rebase", "--skip"],
    "grh": ["git", "reset", "HEAD"],
    "grhh": ["git", "reset", "HEAD", "--hard"],
    "grmv": ["git", "remote", "rename"],
    "grrm": ["git", "remote", "remove"],
    "grset": ["git", "remote", "set-url"],
    "gru": ["git", "reset", "--"],
    "grup": ["git", "remote", "update"],
    "grv": ["git", "remote", "-v"],
    "gsb": ["git", "status", "-sb"],
    "gsd": ["git", "svn", "dcommit"],
    "gsi": ["git", "submodule", "init"],
    "gsps": ["git", "show", "--pretty=short", "--show-signature"],
    "gsr": ["git", "svn", "rebase"],
    "gss": ["git", "status", "-s"],
    "gst": ["git", "status", "-sb"],
    "gsta": ["git", "stash", "save"],
    "gstaa": ["git", "stash", "apply"],
    "gstc": ["git", "stash", "clear"],
    "gstd": ["git", "stash", "drop"],
    "gstl": ["git", "stash", "list"],
    "gstp": ["git", "stash", "pop"],
    "gsts": ["git", "stash", "show", "--text"],
    "gsu": ["git", "submodule", "update"],
    "gtar": ["tar", "-Ipigz", "czfv"],
    "gts": ["git", "tag", "-s"],
    "gtv": ["git", "tag", "|", "sort", "-V"],
    "gunignore": ["git", "update-index", "--no-assume-unchanged"],
    "gunwip": ["git", "log", "-n", "1", "|", "grep", "-q", "-c", "\\-\\-wip\\-\\-", "&&", "git", "reset", "HEAD~1"],
    "gup": ["git", "pull", "--rebase"],
    "gupv": ["git", "pull", "--rebase", "-v"],
    "gwch": ["git", "whatchanged", "-p", "--abbrev-commit", "--pretty=medium"],
    "ipy": ["ipython"],
    "jl": ["julia"],
    "jl.": ["julia", "--project=."],
    "k": ["kubectl"],
    "la": ["ls", "-la"],
    "lh": ["ls", "-lh"],
    "ll": ["ls", "-l"],
    "md": ["mkdir"],
    "npm": ["pnpm"],
    "nv": ["nvim"],
    "pain": ["sudo", "pacman", "-S", "--needed"],
    "painn": ["sudo", "pacman", "-S"],
    "pfc": ["curl", "-F", "c=@-", "http://fars.ee/"],
    "pvim": ["curl", "-F", "vimcn=<-", "https://cfp.vim-cn.com/"],
    "pxy": ["proxychains", "-q"],
    "py": ["python"],
    "restart": ["sudo", "systemctl", "restart"],
    "start": ["sudo", "systemctl", "start"],
    "status": ["sudo", "systemctl", "status"],
    "stop": ["sudo", "systemctl", "stop"],
    "tmux": ["tmux", "-2"],
    "vi": ["vim"],
    "with-github-name": ["GIT_COMMITTER_NAME=BennyYip", "GIT_COMMITTER_EMAIL=yebenmy@protonmail.com", "GIT_AUTHOR_NAME=BennyYip", "GIT_AUTHOR_EMAIL=yebenmy@protonmail.com"],
    "xcp": ["rsync", "-aviHAXKhP", "--delete", "--exclude=*~", "--exclude=__pycache__"],
    "ydcvd": ["ydcv", "-x", "-n", "-t", "2", ">/dev/null"],
}
# fmt:on

aliases['rgg'] = f'bash {$HOME}/dotfiles/bin/rgg'
aliases['agv'] = f'python {$HOME}/dotfiles/bin/agv'

if shutil.which("diff-so-fancy") is not None:
    aliases[
        "dsf"
    ] = "git diff --patience --color=always @($args) | diff-so-fancy | less --tab=4 -RFX"
else:
    aliases["dsf"] = "git diff"


def __add_magnent(
    # fmt:off
    category: Annotated[str, Arg( choices=[ "A", "Game", "TV", "Movie", "Anime", "PhotoBook", "Normal", "Music"])],
    # fmt:on
    url: str,
):
    QB_API = "http://localhost:8964/api/v2"
    data = dict(urls=url, ratioLimit=1.0, category=category, autoTMM="true")
    r = requests.post(f"{QB_API}/torrents/add", data=data)
    print(r.text)


aliases["add-magnent"] = ArgParserAlias(
    func=__add_magnent, has_args=True, threadable=False
)


def find_vcs_root(path=".", *args):
    p = Path(path).absolute()
    markers = [".git", ".svn", ".hg", ".root"]
    while True:
        for marker in markers:
            if (p / marker).exists():
                return p

        parent = p.parent
        if p == parent:
            break

        p = parent
    return None

@aliases.register("zb")
def __zb(args):
    root = find_vcs_root(*args)
    if root:
        os.chdir(root)

aliases["zbi"] = "zb; scd"
aliases["zf"] = "zi"

aliases[".."] = "cd .."

@aliases.register('update')
def __update():
    source ~/.xonshrc

@aliases.register('ols')
def __ols(args):
    python f'{$HOME}/dotfiles/python/open-livestream.py' @(args)

@aliases.register('proxy_on')
def __enable_proxy():
    proxy = "http://127.0.0.1:10809"
    $HTTP_PROXY = proxy
    $HTTPS_PROXY = proxy

@aliases.register('proxy_off')
def __disable_proxy():
    $HTTP_PROXY = ''
    $HTTPS_PROXY = ''

if ON_WINDOWS:
    @aliases.register('ii')
    def __ii(args):
        explorer @(Path(args[0]).absolute())

@aliases.register("ssh")
def __ssh(args):
    with ${...}.swap(TERM="xterm-256color"):
        ssh @(args)
