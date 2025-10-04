alias c='stdoutisatty'

alias vi=vim
alias pxy='proxychains -q'
alias :q="exit"
alias :qa="tmux detach"
alias :e="vim"
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

alias jc='journalctl -xe'
alias jcu='journalctl -xe -u'
alias sc=systemctl
alias scs='systemctl status'
alias scu='systemctl --user'
alias scur='systemctl --user restart'
alias scus='systemctl --user status'
alias ssc='sudo systemctl'
alias sscr='sudo systemctl restart'
alias sscs='sudo systemctl status'
alias rctl='sudo resolvectl'
alias nctl='sudo networkctl'
alias bctl='bluetoothctl'

alias .="source"
alias cp="cp -i --reflink=auto"
alias ssh="TERM=xterm-256color ssh"
alias bc="bc -l"
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

alias vi=vim

alias py=python
alias ipy=ipython
alias bpy=bpython

alias jl=julia
alias jl.="julia --project=."

alias zj='zellij'

if exists eza; then
  xtree() {
    eza -Tl "$@"
  }

  alias exa="eza --group-directories-first --git"
  alias l="eza --group-directories-first -blF --icons=auto"
  alias ll="eza -abghilmu"
  alias llm='ll --sort=modified'
  alias la="LC_COLLATE=C eza -ablF"
  alias tree='eza --tree'
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
  printf '"%s"\n' "$(whoami)@${ipL}:$(realpath -e "$1")"
}

alias cdtmp='cd `mktemp -d /tmp/benyip-XXXXXX`'
alias cd-='cd -'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -='cd -'

alias mkdir="mkdir -pv"
mkcd() {
  mkdir "$1" && cd "$1" || true
}

shutdown() {
  echo -n 你确定要关机吗？
  read -r i
  case "$i" in
    [Yy])
      systemctl poweroff
      ;;
      # dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
  esac
}

if exists pacman; then
  # shellcheck disable=all
  source ~/.shell/arch-alias.sh
elif exists apt; then
  # shellcheck disable=all
  source ~/.shell/debian-alias.sh
fi

y() {
  local tmp
  tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    builtin cd -- "$cwd" || true
  fi
  rm -f -- "$tmp"
}

alias dtop='dune utop'
alias fdall='fd -I -H'

aflfuzz() {
  ASAN_OPTIONS="$ASAN_OPTIONS:symbolize=0" command aflfuzz "$@"
}
afltmin() {
  ASAN_OPTIONS="$ASAN_OPTIONS:symbolize=0" command afltmin "$@"
}
aflcmin() {
  AFL_ALLOW_TMP=1 \
    ASAN_OPTIONS="$ASAN_OPTIONS:symbolize=0" command aflcmin "$@"
}

# shellcheck disable=all
source ~/.shell/git-alias.sh
