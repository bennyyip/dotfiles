alias vi=vim
alias nv=nvim
alias pxy='proxychains -q'
alias :q="exit"
alias :qa="tmux detach"
alias 7z="7z '-xr!*~' '-xr!*.swp'"
alias npm="pnpm"
alias e="emacsclient -t"
alias er="emacsclient -n -s gui"
alias en="emacsclient -n"

alias k="kubectl"

alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -la'
alias lh='ls -lh'
alias grep='grep --color'

alias start="sudo systemctl start"
alias status="sudo systemctl status"
alias stop="sudo systemctl stop"
alias restart="sudo systemctl restart"
alias .="source"
alias cp="cp -i --reflink=auto"
alias ssh="TERM=xterm-256color ssh"
alias bc="bc -l"
alias cower="cower --domain aur.tuna.tsinghua.edu.cn"
alias ydcvd="ydcv -x -n -t 2 >/dev/null"
alias clip="xsel -i -b"

alias gtar="tar -Ipigz czfv"
alias btar="tar -Ilbzip3 cjfv"
alias 7tar="7z a -mmt"
alias xcp="rsync -aviHAXKhP --delete --exclude='*~' --exclude=__pycache__"
alias tmux="tmux -2"
alias urldecode='python2 -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])"'
alias urlencode='python2 -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])"'
alias with-github-name='GIT_COMMITTER_NAME=BennyYip GIT_COMMITTER_EMAIL=yebenmy@protonmail.com GIT_AUTHOR_NAME=BennyYip GIT_AUTHOR_EMAIL=yebenmy@protonmail.com'

alias pvim="curl -F 'vimcn=<-' https://cfp.vim-cn.com/"
alias pfc="curl -F c=@- http://fars.ee/"

alias md=mkdir
alias vi=vim

alias py=python
alias ipy=ipython
alias bpy=bpython

alias jl=julia
alias jl.="julia --project=."

if exists exa; then
  alias l='exa -al'
  xtree() {
    exa -Tl "$@"
  }
else
  alias l='ls -lah --color=auto'
fi

imgvim() {
  curl -F "name=@$1" https://img.vim-cn.com/
}


dsf() {
  # depends on diff-so-fancy
  git diff --patience --color=always $@ | diff-so-fancy | less --tab=4 -RFX
}
alias diff-so-fancy='diff-so-fancy | less'

if exists ghq; then
  alias glook='cd $(ghq list -p | fzf)'
  alias gget='ghq get --no-recursive --shallow'
fi


sshpath() {
  ipL=$(ip -o -4 addr | awk -F "inet |/" '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)
  echo "root@${ipL}:$(realpath -e $1)"
}


# pacman aliases
Syu() {
  sudo pacman -Syu
  pacman -Qtdq | ifne sudo pacman -Rcs -
}

alias Rcs="sudo pacman -Rcs"
alias Ss="pacman -Ss"
alias Si="pacman -Si"
alias Qs="pacman -Qs"
alias Qi="pacman -Qi"
alias Qo="pacman -Qo"
alias Ql="pacman -Ql"
alias Fo="pacman -F"
alias Fy="sudo pacman -Fy"
alias Ssa="pacaur -Ssa"
alias pain='sudo pacman -S --needed'
alias painn='sudo pacmban -S'
alias cower='cower --domain aur.tuna.tsinghua.edu.cn'

paclist() {
  # Source: https://bbs.archlinux.org/viewtopic.php?id=93683
  LC_ALL=C pacman -Qei $(pacman -Qu | cut -d " " -f 1) | \
    awk 'BEGIN {FS=":"} /^Name/{printf("\033[1;36m%s\033[1;37m", $2)} /^Description/{print $2}'
}


# alias Ge="G packages core/extra"
# alias Gc="G community community"
#
# function Ga() { # 獲取PKGBUILD {{{2
#   [ -z "$1" ] && echo "usage: Ga <aur package name>: get AUR package PKGBUILD" && return 1
#   git clone aur@aur.archlinux.org:"$1".git
#   rm -rf "$1"/.git
# }
#
# function G() {
#   [ -z "$3" ] && echo "usage: $0 <$2 package name>: get $2 package PKGBUILD" && return 1
#   git clone https://git.archlinux.org/svntogit/$1.git/ -b packages/$3 --single-branch $3
#   mv "$3"/trunk/* "$3"
#   rm -rf "$3"/{repos,trunk,.git}
# }


shutdown () {
  echo -n 你确定要关机吗？
  read i
  if [[ $i == [Yy] ]]; then
    systemctl poweroff
    # dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
  fi
}

