alias which='(alias; declare -f) | /usr/bin/which --tty-only --read-alias --read-functions --show-tilde --show-dot'

alias vi=vim
alias nv=nvim
alias pxy='proxychains -q'
alias :q="exit"
alias :qa="tmux detach"
alias 7z="7z '-xr!*~' '-xr!*.swp'"
alias npm="pnpm"
alias e="emacsclient -nw"
alias er="emacsclient -n"
alias ec="emacsclient -c -n"

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

alias dsf='git diff'

if exists ghq; then
  alias glook='cd $(ghq list -p | fzf)'
  alias gget='ghq get --no-recursive --shallow'
fi

sshpath() {
  ipL=$(ip -o -4 addr | awk -F "inet |/" '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)
  echo "root@${ipL}:$(realpath -e $1)"
}

alias cdtmp='cd `mktemp -d /tmp/benyip-XXXXXX`'

shutdown() {
  echo -n 你确定要关机吗？
  read i
  if [[ $i == [Yy] ]]; then
    systemctl poweroff
    # dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
  fi
}

if exists pacman; then
  source ~/.shell/arch-alias.sh
elif exists apt; then
  source ~/.shell/debian-alias.sh
fi
