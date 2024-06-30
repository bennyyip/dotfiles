import os
import time
import shutil
from pathlib import Path
import webbrowser
import tempfile

import httpx

from xonsh.tools import unthreadable
from xonsh import dirstack
from xonsh.platform import ON_WINDOWS, ON_LINUX
from xonsh.cli_utils import Annotated, Arg, ArgParserAlias
import pyperclip

from typing import Optional

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
        "paiN": ["sudo", "pacman", "-S"],
    }

    @aliases.register("Syu")
    def __Syu(args):
        sudo pacman -Syu
        pacman -Qtdq | ifne sudo pacman -Rcs -
elif ON_WINDOWS:
    aliases |= {
      'Syu': 'gsudo winget upgrade --all --verbose',
      "Ss": ['winget', 'search'],
      "pain": ['winget', 'install'],
      "paiN": ['winget', 'install', '--force'],
    }


aliases |= {
    "scp-resume": ["rsync", "--partial", "-h", "--progress", "--rsh=ssh"],
    "ipynb": ["jupyter", "notebook", "--no-browser"],
    "grep": ["grep", "--color=auto"],
    "egrep": ["egrep", "--color=auto"],
    "fgrep": ["fgrep", "--color=auto"],
    "l": "ls -lah --color=auto",
    "ls": ["ls", "--color=auto", "-v"],
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
    # "ytdl": ["yt-dlp", "--downloader", "aria2c"],
    "ytdl": ["yt-dlp"],
    'ytdl-sub': ["ytdl", "--write-sub", "--write-auto-sub", "--skip-download", "--no-write-thumbnai", "--no-embed-thumbnail", '--convert-subs', 'vtt', "-P", "subtitle:subs", "-o", "subtitle:%(extractor)s-%(id)s.%(ext)s"],
    'strip-vtt': ['rg', '-v', r'^((WEBVTT)|((\d\d:){1,2}\d\d.\d+ --> (\d\d:){1,2}\d\d.\d+))?$'],
}
# fmt:on

aliases['rgg'] = f'bash {$HOME}/dotfiles/bin/rgg'
aliases['agv'] = f'python {$HOME}/dotfiles/bin/agv'
aliases['to-utf8'] = ["vim", "--clean", '+set nobomb | set fenc=utf8 | x']

if shutil.which("diff-so-fancy") is not None:
    aliases[
        "dsf"
    ] = "git diff --patience --color=always @($args) | diff-so-fancy | less --tab=4 -RFX"
else:
    aliases["dsf"] = "git diff"


if shutil.which("eza") is not None:
    aliases |= {
      'l': 'eza',
      'la': ['eza', '--all'],
      'll': ['eza', '--long'],
      'laht': ['eza', '--all', '--long', '--sort=modified'],
    }

def __add_magnet(
    # fmt:off
    category: Annotated[str, Arg(choices=[ "A", "Game", "TV", "Movie", "Anime", "PhotoBook", "Normal", "Music"])],
    # fmt:on
    # url: Annotated[Optional[str], Arg(nargs='?')],
    url: Optional[str] = None,
):
    if url is None:
        url = pyperclip.paste()
        if not url.startswith('magnet:?'):
            print("No valid magnet url in clipboard.")
            return

    QB_API = "http://localhost:8964/api/v2"
    data = dict(urls=url, ratioLimit=1.0, category=category, autoTMM="true")
    r = httpx.post(f"{QB_API}/torrents/add", data=data)
    print(r.text)


aliases["add-magnet"] = ArgParserAlias(func=__add_magnet, has_args=True, threadable=False)



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

@aliases.register("cdp")
def __cdp(args):
    dirstack.cd([Path(args[0]).parent])

@aliases.register('update')
def __update():
    source ~/.xonshrc

@aliases.register('ols')
def __ols(args):
    python f'{$HOME}/dotfiles/python/open-livestream.py' @(args)

@aliases.register('proxy_on')
def __enable_proxy():
    proxy = $MY_PROXY
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

    aliases['powershell'] = 'pwsh'
    aliases['pwshn'] = ['pwsh', '-NoProfile', '-NoLogo']
    aliases['sudo'] = 'gsudo'
    aliases['vimdiff'] = ['vim', '-O', '+windo diffthis']
    aliases['gvimdiff'] = ['gvim', '-O', '+windo diffthis']

@aliases.register("ssh")
def __ssh(args):
    with ${...}.swap(TERM="xterm-256color"):
        ssh @(args)

@aliases.register("clp")
def __clp(args, stdin, stdout):
    if stdin is None:
        print('No input. Example: `echo hello | clp`')
        return -1
    txt = ''
    for line in stdin.readlines():
        txt += line
    pyperclip.copy(txt)
    return 0

def __extract_subtitle(
        input_file: str,
        output_file: str,
        lang: Annotated[Optional[str], Arg(nargs='?')] = 'eng',
        interative: Annotated[Optional[bool], Arg('--interative', '-i')] = False):
    if interative:
        s = $(ffprobe -hide_banner -i @(input_file)  2>&1 | grep Stream | grep Subtitle | fzf)
        s = re.search(r'#(\d:\d+)', s)[1]
        ffmpeg -hide_banner -i @(input_file)  -map  @(s) -vn -an @(output_file)
    else:
        ffmpeg -hide_banner -i @(input_file)  -map  f'0:m:language:{lang}' -vn -an @(output_file)

aliases["extract-subtitle"] = ArgParserAlias(func=__extract_subtitle, has_args=True, threadable=False)

@unthreadable
@aliases.register("vimv")
def __vimv(args):
    try:
        fd, fname = tempfile.mkstemp(prefix='vimv', text=True)
        src = $(ls @(args))
        with os.fdopen(fd, 'w') as f:
            f.write(src)
        rtn = !(vim @(fname)).rtn
        if rtn != 0:
            print(f"WARN: Vim exit code {rtn}. Aborting..")
            return
        dst = $(cat @(fname))
        src, dst = src.split('\n'), dst.split('\n')
        if len(src) != len(dst):
            print("WARN: Number of files changed. Did you delete a line by accident? Aborting..")
        count = 0
        for s, d in zip(src, dst):
            if s == d:
                continue
            Path(d).parent.mkdir(exist_ok=True, parents=True)
            os.rename(s, d)
            count += 1
        print(f"{count} files renamed.")
    except:
        pass
    finally:
        os.unlink(fname)

@aliases.register('open-video-url')
def __open_video_url(args):
    # For ytdl videos
    url = $(ffprobe @(args[0]) 2>&1 | rg comment| awk '{print $3}')
    print(url)
    webbrowser.open(url)

@aliases.register('clp')
def __clp(args, stdin, stdout):
    if stdin is None:
        print('No input. Example: `echo hello | clp`')
        return -1
    txt = ''
    for line in stdin.readlines():
        txt += line
    pyperclip.copy(txt)
    return 0

@unthreadable
@aliases.register('set-brightness')
def __set_brightness(args):
    monitorcontrol --set-luminance @(args)

aliases['turn-off-monitors'] = 'monitorcontrol --set-power-mode off_hard'


@unthreadable
@aliases.register('sync-subs')
def __sync_subs(args):
    if (len(args) == 0):
        videos = pg`*.mkv`
    else:
        videos = [Path(x) for x in args]
        assert all((x.is_file() for x in videos))
    for v in videos:
        for s in p`.+.(ass|srt)`:
            if v.stem in s.stem:
                ffsubsync -i @(s) --overwrite-input @(v)


@aliases.register('backup-files')
def __sync_subs(args):
    os.makedirs('bak', exist_ok=True)
    files = [Path(x) for x in args]
    assert all((x.is_file() for x in files))
    for x in files:
        shutil.copy2(x, 'bak')

try:
    import pysubs2

    def __align_sub(
        i: str,
        r: Annotated[str, Arg(help="reference subtitle")],
        inidx: Annotated[Optional[int], Arg("--inidx", type=int)] = 0,
        refidx: Annotated[Optional[int], Arg("--refidx", type=int)] = 0,
        o: Annotated[Optional[str], Arg("-o", "--output", nargs="?")] = None,
        dryrun: Annotated[Optional[bool], Arg("--dry-run", "-d")] = False,
    ):
        insub = pysubs2.load(i)
        refsub = pysubs2.load(r)
        offset = refsub[refidx].start - insub[inidx].start
        insub.shift(ms=offset)
        if inidx != 0 or refidx !=0:
            print(f'in[{inidx}]: {insub[inidx].text}')
            print(f'ref[{refidx}]: {refsub[refidx].text}')
        print('offset in ms:', offset)
        if dryrun:
            return
        if o is not None:
            insub.save(o)
        else:
            insub.save(i)


    aliases["align-sub"] = ArgParserAlias(func=__align_sub, has_args=True, threadable=False)
except ImportError:
    pass


@unthreadable
@aliases.register('yy')
def __yazi_cd(args):
    try:
        fd, tmp = tempfile.mkstemp(prefix='yazi-cwd', text=True)
        yazi @(args) --cwd-file=@(tmp)
        with os.fdopen(fd, 'r') as f:
            cwd = f.read().strip()
        if cwd != '' and Path(cwd).exists() and cwd != $PWD:
                dirstack.cd([cwd])
    finally:
        os.remove(tmp)

@unthreadable
@aliases.register('yz')
def __yazi_z(args):
    yy @$(zoxide query @(args))

@unthreadable
@aliases.register('yzi')
@aliases.register('yzf')
def __yazi_zi(args):
    yy @$(zoxide query -i @(args))


@unthreadable
@aliases.register('vr')
def __gvim_remote(args):
    gvim --remote-silent-tab @(args)


@unthreadable
@aliases.register('view-sub')
def __view_sub(args):
    x = $(ytdl-sub @(args))
    strip-vtt @(x.split('\n')[-2].split('"')[-2]) | bat
