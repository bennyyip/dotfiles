# 确定环境 {{{1
OS=${$(uname)%_*}
if [[ $OS == "CYGWIN" || $OS == "MSYS" ]]; then
  OS=Linux
elif [[ $OS == "Darwin" ]]; then
  OS=FreeBSD
fi
if [ -f /etc/issue ] && grep -qis 'Arch Linux' /etc/issue; then
  IS_ARCH=1
else
  IS_ARCH=0
fi
# check first, or the script will end wherever it fails
zmodload zsh/regex 2>/dev/null && _has_re=1 || _has_re=0
unsetopt nomatch
zmodload zsh/subreap 2>/dev/null && subreap
_zdir=${ZDOTDIR:-$HOME}
HISTFILE=${_zdir}/.histfile
HISTSIZE=10000
SAVEHIST=10000

ZSH_CACHE_DIR=$_zdir/.ZSH_CACHE_DIR
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

zstyle :compinstall filename "$_zdir/.zshrc"
#fpath=($_zdir/.zsh/Completion $_zdir/.zsh/functions $fpath)
fpath=($_zdir/.zsh/Completion $_zdir/.zsh/functions $fpath)
autoload -Uz compinit
compinit

# 變量設置 {{{1
[[ -z $EDITOR ]] && (( $+commands[vim] )) && export EDITOR=vim
export RUST_SRC_PATH=~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/
export PATH=~/.cargo/bin/:~/.local/bin:$PATH

# 图形终端下(包括ssh登录时)的设置{{{2
if [[ -n $DISPLAY && -z $SSH_CONNECTION ]]; then
  export BROWSER=firefox
  export wiki_browser=firefox
  export AGV_EDITOR='vv ''$file:$line:$col'''
else
  export AGV_EDITOR='vim +"call setpos(\".\", [0, $line, $col, 0])" ''$file'''
fi
if [[ -n $DISPLAY || -n $SSH_CONNECTION ]]; then
  # 让 less 将粗体/下划线等显示为彩色
  export LESS_TERMCAP_mb=$'\x1b[01;31m'
  export LESS_TERMCAP_md=$'\x1b[01;38;5;74m'
  export LESS_TERMCAP_me=$'\x1b[0m'
  export LESS_TERMCAP_se=$'\x1b[0m'
  export LESS_TERMCAP_so=$'\x1b[7m'
  export LESS_TERMCAP_ue=$'\x1b[0m'
  export LESS_TERMCAP_us=$'\x1b[04;38;5;146m'

  if [[ $TERM == linux ]]; then
    _256colors=0
  else
    [[ $TERM != *color* ]] && export TERM=${TERM%%[.-]*}-256color
    _256colors=1
  fi
else
  # tty 下光标显示为块状
  echo -ne "\e[?6c"
  zshexit () {
    [[ $SHLVL -eq 1 ]] && echo -ne "\e[?0c"
  }
  [[ $TERM == *color* ]] && _256colors=1
fi

if [[ $OS = Linux ]]; then
  # under fbterm
  # can't see parent on some restricted systems
  if [[ $_has_re -eq 1 &&
    $(</proc/$PPID/cmdline) =~ '(^|/)fbterm' ]] 2>/dev/null; then
  export TERM=fbterm
  echo hh
  export LANG=zh_CN.UTF-8
  # This term is quirk. ls doesn't like it.
  # _256colors=1
fi
if [[ $_256colors -eq 1 ]]; then
  export LS_COLORS='rs=0:di=38;5;27:ln=38;5;51:mh=44;38;5;15:pi=40;38;5;11:so=38;5;13:do=38;5;5:bd=48;5;232;38;5;11:cd=48;5;232;38;5;3:or=48;5;232;38;5;9:mi=05;48;5;232;38;5;15:su=48;5;196;38;5;15:sg=48;5;11;38;5;16:ca=48;5;196;38;5;226:tw=48;5;10;38;5;16:ow=48;5;10;38;5;21:st=48;5;21;38;5;15:ex=38;5;34:*.tar=38;5;9:*.tgz=38;5;9:*.arc=38;5;9:*.arj=38;5;9:*.taz=38;5;9:*.lha=38;5;9:*.lzh=38;5;9:*.lzma=38;5;9:*.tlz=38;5;9:*.txz=38;5;9:*.tzo=38;5;9:*.t7z=38;5;9:*.zip=38;5;9:*.z=38;5;9:*.Z=38;5;9:*.dz=38;5;9:*.gz=38;5;9:*.lrz=38;5;9:*.lz=38;5;9:*.lzo=38;5;9:*.xz=38;5;9:*.bz2=38;5;9:*.bz=38;5;9:*.tbz=38;5;9:*.tbz2=38;5;9:*.tz=38;5;9:*.deb=38;5;9:*.rpm=38;5;9:*.jar=38;5;9:*.war=38;5;9:*.ear=38;5;9:*.sar=38;5;9:*.rar=38;5;9:*.alz=38;5;9:*.ace=38;5;9:*.zoo=38;5;9:*.cpio=38;5;9:*.7z=38;5;9:*.rz=38;5;9:*.cab=38;5;9:*.jpg=38;5;13:*.JPG=38;5;13:*.jpeg=38;5;13:*.gif=38;5;13:*.bmp=38;5;13:*.pbm=38;5;13:*.pgm=38;5;13:*.ppm=38;5;13:*.tga=38;5;13:*.xbm=38;5;13:*.xpm=38;5;13:*.tif=38;5;13:*.tiff=38;5;13:*.png=38;5;13:*.svg=38;5;13:*.svgz=38;5;13:*.mng=38;5;13:*.pcx=38;5;13:*.mov=38;5;13:*.mpg=38;5;13:*.mpeg=38;5;13:*.m2v=38;5;13:*.mkv=38;5;13:*.ogm=38;5;13:*.mp4=38;5;13:*.m4v=38;5;13:*.mp4v=38;5;13:*.vob=38;5;13:*.qt=38;5;13:*.nuv=38;5;13:*.wmv=38;5;13:*.asf=38;5;13:*.rm=38;5;13:*.rmvb=38;5;13:*.flc=38;5;13:*.avi=38;5;13:*.fli=38;5;13:*.flv=38;5;13:*.webm=38;5;13:*.gl=38;5;13:*.dl=38;5;13:*.xcf=38;5;13:*.xwd=38;5;13:*.yuv=38;5;13:*.cgm=38;5;13:*.emf=38;5;13:*.axv=38;5;13:*.anx=38;5;13:*.ogv=38;5;13:*.ogx=38;5;13:*.aac=38;5;45:*.au=38;5;45:*.flac=38;5;45:*.mid=38;5;45:*.midi=38;5;45:*.mka=38;5;45:*.mp3=38;5;45:*.m4a=38;5;45:*.mpc=38;5;45:*.ogg=38;5;45:*.opus=38;5;45:*.3gp=38;5;45:*.ra=38;5;45:*.wav=38;5;45:*.axa=38;5;45:*.oga=38;5;45:*.spx=38;5;45:*.xspf=38;5;45:*~=38;5;244:'
else
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi
unset _256colors
unset _has_re

export LESS="-FRXM"
# default has -S
export SYSTEMD_LESS="${LESS#-}K"

# 选项设置{{{1
unsetopt beep
# 不需要打 cd，直接进入目录
setopt autocd
# 自动记住已访问目录栈
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus
# rm * 时不要提示
setopt rm_star_silent
# 允许在交互模式中使用注释
setopt interactive_comments
# disown 后自动继续进程
setopt auto_continue
setopt extended_glob
# 单引号中的 '' 表示一个 ' （如同 Vimscript 中者）
setopt rc_quotes
# 补全列表不同列可以使用不同的列宽
setopt listpacked
# 补全 identifier=path 形式的参数
setopt magic_equal_subst
# 为方便复制，右边的提示符只在最新的提示符上显示
setopt transient_rprompt
# setopt promptsubst
setopt promptsubst
# setopt 的输出显示选项的开关状态
setopt ksh_option_print
setopt no_bg_nice
setopt noflowcontrol
stty -ixon # 上一行在 tmux 中不起作用

# 历史记录{{{2
# 不保存重复的历史记录项
setopt hist_save_no_dups
setopt hist_ignore_dups
# setopt hist_ignore_all_dups
# 在命令前添加空格，不将此命令添加到记录文件中
setopt hist_ignore_space
# zsh 4.3.6 doesn't have this option
setopt hist_fcntl_lock 2>/dev/null
if [[ $_has_re -eq 1 &&
  ! ( $ZSH_VERSION =~ '^[0-4]\.' || $ZSH_VERSION =~ '^5\.0\.[0-4]' ) ]]; then
    setopt hist_reduce_blanks
else
  # This may cause the command messed up due to a memcpy bug
fi

# 补全与 zstyle {{{1
# 自动补全 {{{2
# 用本用户的所有进程补全
zstyle ':completion:*:processes' command 'ps -afu$USER'
zstyle ':completion:*:*:*:*:processes' force-list always
# 进程名补全
zstyle ':completion:*:processes-names' command  'ps c -u ${USER} -o command | uniq'

# 警告显示为红色
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
# 描述显示为淡色
zstyle ':completion:*:descriptions' format $'\e[2m -- %d --\e[0m'
zstyle ':completion:*:corrections' format $'\e[01;33m -- %d (errors: %e) --\e[0m'

# cd 补全顺序
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
# 在 .. 后不要回到当前目录
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# complete manual by their section, from grml
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true

zstyle ':completion:*' menu select
# 分组显示
zstyle ':completion:*' group-name ''
# 歧义字符加粗（使用「true」来加下划线）；会导致原本的高亮失效
# http://www.thregr.org/~wavexx/rnd/20141010-zsh_show_ambiguity/
# zstyle ':completion:*' show-ambiguity '1;37'
# 在最后尝试使用文件名
zstyle ':completion:*' completer _complete _match _approximate _expand_alias _ignored _files
# 修正大小写
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle -e ':completion:*' special-dirs \
  '[[ $PREFIX == (../)#(|.|..) ]] && reply=(..)'
# 使用缓存。某些命令的补全很耗时的（如 aptitude）
zstyle ':completion:*' use-cache on
_cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}/zsh
zstyle ':completion:*' cache-path $_cache_dir
unset _cache_dir

# complete user-commands for git-*
# https://pbrisbin.com/posts/deleting_git_tags_with_style/
zstyle ':completion:*:*:git:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}

compdef pkill=killall
compdef pgrep=killall
compdef vman=man
compdef proxychains=command
compdef watch=command
compdef rlwrap=command
compdef ptyless=command
compdef grc=command
compdef agg=ag 2>/dev/null
compdef rgg=rg 2>/dev/null
compdef downgrade=pactree 2>/dev/null
# not only pdf files
compdef -d zathura
compdef _gnu_generic exa pamixer
compdef whoneeds=pactree 2>/dev/null

# 我的自动补全 {{{2
zstyle ':completion:*:*:pdf2png:*' file-patterns \
  '*.pdf:pdf-files:pdf\ files *(-/):directories:directories'
zstyle ':completion:*:*:x:*' file-patterns \
  '*.{7z,bz2,gz,rar,tar,tbz,tgz,zip,chm,xz,exe,xpi,apk,maff,crx}:compressed-files:compressed\ files *(-/):directories:directories'
zstyle ':completion:*:*:zathura:*' file-patterns \
  '*.{pdf,ps,eps,dvi,djvu,pdf.gz,ps.gz,dvi.gz}:documents:documents *(-/):directories:directories'
zstyle ':completion:*:*:gbkunzip:*' file-patterns '*.zip:zip-files:zip\ files *(-/):directories:directories'
zstyle ':completion:*:*:flashplayer:*' file-patterns '*.swf'
zstyle ':completion:*:*:hp2ps:*' file-patterns '*.hp'
zstyle ':completion:*:*:feh:*' file-patterns '*.{png,gif,jpg,svg}:images:images *(-/):directories:directories'
zstyle ':completion:*:*:sxiv:*' file-patterns '*.{png,gif,jpg}:images:images *(-/):directories:directories'
zstyle ':completion:*:*:timidity:*' file-patterns '*.mid'

# 命令行编辑{{{1
bindkey -e

# ^Xe 用$EDITOR编辑命令
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

zle -C complete-file menu-expand-or-complete _generic
zstyle ':completion:complete-file:*' completer _files

# https://zhimingwang.org/blog/2015-09-21-zsh-51-and-bracketed-paste.html
autoload -Uz bracketed-paste-url-magic
zle -N bracketed-paste bracketed-paste-url-magic

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic
toggle-uqm () {
if zle -l self-insert; then
  zle -A .self-insert self-insert && zle -M "switched to self-insert"
else
  zle -N self-insert url-quote-magic && zle -M "switched to url-quote-magic"
fi
}
zle -N toggle-uqm
bindkey '^X$' toggle-uqm

# better than copy-prev-word
bindkey "^[^_" copy-prev-shell-word

insert-last-word-r () {
zle insert-last-word -- 1
}
zle -N insert-last-word-r
bindkey "\e_" insert-last-word-r
# Not works with my insert-last-word-r
# autoload -Uz smart-insert-last-word
# zle -N insert-last-word smart-insert-last-word
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey '\e=' copy-earlier-word

zmodload zsh/complist
bindkey -M menuselect '^O' accept-and-infer-next-history

bindkey "^X^I" complete-file
bindkey "^X^f" complete-file
bindkey "^U" backward-kill-line
bindkey "^]" vi-find-next-char
bindkey "\e]" vi-find-prev-char
bindkey "\eq" push-line-or-edit
bindkey -s "\e[Z" "^P"
bindkey '^Xa' _expand_alias
bindkey '^[/' _history-complete-older
bindkey '\e ' set-mark-command
# 用单引号引起最后一个单词
# FIXME 如何引起光标处的单词？
bindkey -s "^['" "^[] ^f^@^e^[\""
# 打开 zsh 的PDF格式文档
bindkey -s "^X^D" "zathura ~/文档/编程/shell/zsh/zsh.pdf &^M"
bindkey -s "^Xc" "tmux attach -d^M"

bindkey '^[p' up-line-or-search
bindkey '^[n' down-line-or-search

# restoring an aborted command-line {{{2
# unsupported with 4.3.17
if zle -la split-undo; then
  zle-line-init () {
  if [[ -n $ZLE_LINE_ABORTED ]]; then
    _last_aborted_line=$ZLE_LINE_ABORTED
  fi
  if [[ -n $_last_aborted_line ]]; then
    local savebuf="$BUFFER" savecur="$CURSOR"
    BUFFER="$_last_aborted_line"
    CURSOR="$#BUFFER"
    zle split-undo
    BUFFER="$savebuf" CURSOR="$savecur"
  fi
}
zle -N zle-line-init
zle-line-finish() {
unset _last_aborted_line
  }
  zle -N zle-line-finish
fi
# move by shell word {{{2
# stop at /
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
zsh-word-movement () {
  # see select-word-style for more
  local -a word_functions
  local f

  word_functions=(backward-kill-word backward-word
  capitalize-word down-case-word
  forward-word kill-word
  transpose-words up-case-word)

  if ! zle -l $word_functions[1]; then
    for f in $word_functions; do
      autoload -Uz $f-match
      zle -N zsh-$f $f-match
    done
  fi
  # set the style to shell
  zstyle ':zle:zsh-*' word-style shell
}
zsh-word-movement
unfunction zsh-word-movement
bindkey "\eB" zsh-backward-word
bindkey "\eF" zsh-forward-word
bindkey "\eW" zsh-backward-kill-word

# C-x p 在当前/上一条命令前插入 proxychains -q {{{2
prefix-proxy() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != proxychains\ * && $UID -ne 0 ]] && {
      typeset -a bufs
      bufs=(${(z)BUFFER})
      while (( $+aliases[$bufs[1]] )); do
        local expanded=(${(z)aliases[$bufs[1]]})
        bufs[1,1]=($expanded)
        if [[ $bufs[1] == $expanded[1] ]]; then
          break
        fi
      done
      bufs=(proxychains -q $bufs)
      BUFFER=$bufs
    }
    zle end-of-line
}
zle -N prefix-proxy
bindkey "^Xp" prefix-proxy

# Esc-Esc 在当前/上一条命令前插入 sudo {{{2
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * && $UID -ne 0 ]] && {
      typeset -a bufs
      bufs=(${(z)BUFFER})
      while (( $+aliases[$bufs[1]] )); do
        local expanded=(${(z)aliases[$bufs[1]]})
        bufs[1,1]=($expanded)
        if [[ $bufs[1] == $expanded[1] ]]; then
          break
        fi
      done
      bufs=(sudo $bufs)
      BUFFER=$bufs
    }
    zle end-of-line
}
zle -N sudo-command-line
bindkey "\e\e" sudo-command-line
# 插入当前的所有补全 http://www.zsh.org/mla/users/2000/msg00601.html {{{2
_insert_all_matches () {
  setopt localoptions nullglob rcexpandparam extendedglob noshglob
  unsetopt markdirs globsubst shwordsplit nounset ksharrays
  compstate[insert]=all
  compstate[old_list]=keep
  _complete
}
zle -C insert-all-matches complete-word _insert_all_matches
bindkey '^Xi' insert-all-matches
# key bindings fixes for urxvt
#bindkey "^[[7~" beginning-of-line
#bindkey "^[[8~" end-of-line
#bindkey "^[[5~" beginning-of-history
#bindkey "^[[6~" end-of-history
bindkey "^[[3~" delete-char
bindkey "^[[2~" quoted-insert

# 函數 {{{1
autoload zargs
autoload zmv
TRAPTERM () { exit }
update () { . $_zdir/.zshrc }
if (( $+commands[vimtrace] )); then
  (( $+commands[strace] )) && alias strace='vimtrace strace'
  (( $+commands[ltrace] )) && alias ltrace='vimtrace ltrace'
else
  (( $+commands[strace] )) && strace () { (command strace "$@" 3>&1 1>&2 2>&3) | vim -R - }
  (( $+commands[ltrace] )) && ltrace () { (command ltrace "$@" 3>&1 1>&2 2>&3) | vim -R - }
fi
vman () { vim +"set ft=man" +"Man $*" }
mvpc () { mv $1 "`echo $1|ascii2uni -a J`" } # 将以 %HH 表示的文件名改正常
nocolor () { sed -r "s:\x1b\[[0-9;]*[mK]::g" }
sshpubkey () { tee < ~/.ssh/id_*.pub(om[1]) >(xsel -ib) }
sshpath() {
  ipL=$(ip -o -4 addr | awk -F "inet |/" '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)
  echo "root@${ipL}:$(realpath -e $1)"
}
function Ga() { # 獲取PKGBUILD {{{2
  [ -z "$1" ] && echo "usage: Ga <aur package name>: get AUR package PKGBUILD" && return 1
  git clone aur@aur.archlinux.org:"$1".git
  rm -rf "$1"/.git
}

function G() {
  [ -z "$3" ] && echo "usage: $0 <$2 package name>: get $2 package PKGBUILD" && return 1
  git clone https://git.archlinux.org/svntogit/$1.git/ -b packages/$3 --single-branch $3
  mv "$3"/trunk/* "$3"
  rm -rf "$3"/{repos,trunk,.git}
}

alias Ge="G packages core/extra"
alias Gc="G community community"

breakln () { #断掉软链接 {{{2
  for f in $*; do
    tgt=$(readlink "$f")
    unlink "$f"
    cp -rL "$tgt" "$f"
  done
}

try_until_succeed () { #反复重试，直到成功 {{{2
  while ! $*; do :; done
}
rmempty () { #删除空文件 {{{2
  for i; do
    [[ -f $i && ! -s $i ]] && rm $i
  done
  return 0
}

if [[ -d ${VIMTMP:=/tmp} ]]; then # {{{2 gcc & g++
  gcc () { # {{{3
    errfile=$VIMTMP/.error
    command gcc -g -Wall "$@" >$errfile 2>&1
    ret=$?
    cat $errfile
    return $ret
  }
  g++ () { # {{{3
  errfile=$VIMTMP/.error
  command g++ -g -Wall "$@" >$errfile 2>&1
  ret=$?
  cat $errfile
  return $ret
}
clang () { # {{{3
  errfile=$VIMTMP/.error
  command clang -g -Wall "$@" >$errfile 2>&1
  ret=$?
  cat $errfile
  return $ret
}
clang++ () { # {{{3
errfile=$VIMTMP/.error
command clang++ -g -Wall "$@" >$errfile 2>&1
ret=$?
cat $errfile
return $ret
}
fi
duppkg4repo () { #软件仓库中重复的软件包 {{{2
  local repo=$1
  [[ -z $repo ]] && { echo >&2 'which repository to examine?'; return 1 }
  local pkgs
  pkgs=$(comm -12 \
    <(pacman -Sl $repo|awk '{print $2}'|sort) \
    <(pacman -Sl|awk -vrepo=$repo '$1 != repo {print $2}'|sort) \
    )
  [[ -z $pkgs ]] && return 0
  LANG=C pacman -Si ${=pkgs} | awk -vself=$repo '/^Repository/{ repo=$3; } /^Name/ && repo != self { printf("%s/%s\n", repo, $3); }'
}
iip () { #{{{2
  qip=${1:-cip}
  echo -n "ip> "
  read ip
  while [[ $ip != 'q' ]]; do
    $qip $ip
    echo -n "ip> "
    read ip
  done
  unset ip
}
pid () { #{{{2
  s=0
  for i in $*; do
    echo -n "$i: "
    r=$(cat /proc/$i/cmdline|tr '\0' ' ' 2>/dev/null)
    if [[ $? -ne 0 ]]; then
      echo not found
      s=1
    else
      echo $r
    fi
  done
  return $s
}
en () { # 使用 DNS TXT 记录的词典 {{{2
  # https://github.com/chuangbo/jianbing-dictionary-dns
  dig "$*.jianbing.org" +short txt | perl -pe's/\\(\d{1,3})/chr $1/eg; s/(^"|"$)//g'
}
shutdown () { #{{{2
  echo -n 你确定要关机吗？
  read i
  if [[ $i == [Yy] ]]; then
    systemctl poweroff
    # dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
  fi
}
killssh () { #{{{2 kill ssh that using default master socket
  local keys
  if [[ $# -le 1 ]]; then
    keys=('')
  else
    keys=$@
  fi

  for key in "${keys[@]}"; do
    local pids="$(netstat -nxlp 2>/dev/null | awk -v dir=$(_killssh_dir) -v key=$key \
      '{if(index($NF, dir"/master-"key) == 1){print $9}}' | grep -o '^[[:digit:]]\+')"
    [[ -n $pids ]] && kill ${=pids}
  done
}

_killssh_dir () {
  local dir
  if [[ -n $XDG_RUNTIME_DIR && -d $XDG_RUNTIME_DIR/ssh ]]; then
    dir=$XDG_RUNTIME_DIR/ssh
  else
    dir=$HOME/.ssh
  fi
  print $dir
}

_killssh_items () {
  netstat -nxlp 2>/dev/null | awk -v dir=$(_killssh_dir) \
    'BEGIN{P=dir"/master-";L=length(P);}{if(index($NF, P) == 1){a=substr($NF,L+1);sub(/\.[[:alnum:]]+$/,"",a);print a}}'
}

_killssh () {
  _arguments \
    ':what:($(_killssh_items))'
  return 0
}
compdef _killssh killssh
mvgb () { # 文件名从 GB 转码，带确认{{{2
  for i in $*; do
    new="`echo $i|iconv -f utf8 -t latin1|iconv -f gbk`"
    echo $new
    echo -n 'Sure? '
    read -q ans && mv -i $i $new
    echo
  done
}
ptyrun () { # 使用伪终端代替管道，对 ls 这种“顽固分子”有效 {{{2
  local ptyname=pty-$$
  zmodload zsh/zpty
  zpty $ptyname "${(q)@}"
  if [[ ! -t 1 ]]; then
    setopt local_traps
    trap '' INT
  fi
  zpty -r $ptyname
  zpty -d $ptyname
}
ptyless () {
  ptyrun "$@" | tr -d $'\x0f' | less
}
screen2clipboard () { # 截图到剪贴板 {{{2
  import png:- | xclip -i -selection clipboard -t image/png
}
if [[ $TERM == xterm* || $TERM == *rxvt* ]]; then # {{{2 设置光标颜色
  cursorcolor () { echo -ne "\e]12;$*\007" }
elif [[ $TERM == screen* ]]; then
  if (( $+TMUX )); then
    cursorcolor () { echo -ne "\ePtmux;\e\e]12;$*\007\e\\" }
  else
    cursorcolor () { echo -ne "\eP\e]12;$*\007\e\\" }
  fi
elif [[ $TERM == tmux* ]]; then
  cursorcolor () { echo -ne "\ePtmux;\e\e]12;$*\007\e\\" }
fi

# GPG
function gpg_restart {
  pkill gpg
  pkill pinentry
  pkill ssh-agent
  eval $(gpg-agent --daemon --enable-ssh-support) }

function secret {
  output="${HOME}/$(basename ${1}).$(date +%F).enc"
  gpg --encrypt --armor \
    --output ${output} \
    -r $KEYID \
    "${1}" && echo "${1} -> ${output}" }

function reveal {
  output=$(echo "${1}" | rev | cut -c16- | rev)
  gpg --decrypt --output ${output} "${1}" \
    && echo "${1} -> ${output}" }




# 別名 {{{1
alias vi=vim
alias nv=nvim
alias pxy='proxychains -q'
alias :q="exit"
alias :qa="tmux detach"
alias 7z="7z '-xr!*~' '-xr!*.swp'"

alias k="kubectl"

(($+commands[exa])) && {
  alias e='exa'
  xtree () {
    exa -Tl "$@"
  }
  alias l='exa -al'
} || {
  alias l='ls -lah --color=auto'
}

alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -la'
alias lh='ls -lh'
alias grep='grep --color'

function scrotclip() {
  scrot -s -e 'xclip -selection clipboard -t "image/png" < $f'
}


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
imgvim(){
  curl -F "name=@$1" https://img.vim-cn.com/
}


dsf(){
  # depends on diff-so-fancy
  git diff --patience --color=always $@ | diff-so-fancy | less --tab=4 -RFX
}

alias md=mkdir
alias which='(alias; declare -f) | /usr/bin/which --tty-only --read-alias --read-functions --show-tilde --show-dot'
alias vi=vim

alias py=python
alias ipy=ipython
alias bpy=bpython

[ $commands[ghq] ]  && {
  alias glook='cd $(ghq list -p | sk)'
  alias gget='ghq get'
}
(( $+commands[diff-so-fancy] )) && alias diff-so-fancy='diff-so-fancy | less'
# 後綴別名 {{{2
alias -s pdf=zathura
alias -s {jpg,png,gif}=feh
alias -s tar="tar -xvf"
alias -s {tgz,gz}="tar -xvzf"
alias -s bz2="tar -xvjf"
alias -s zip=unzip
alias -s epub="emacsclient -n"

# pacman aliases and functions {{{2
function Syu(){
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

alias upvim="vim +PlugUpgrade +PlugUpdate"

# 全局别名 {{{2
# 当前目录下最后修改的文件
# 来自 http://roylez.heroku.com/2010/03/06/zsh-recent-file-alias.html
alias -g NN="*(oc[1])"
alias -g NNF="*(oc[1].)"
alias -g NUL="/dev/null"
alias -g XS='"$(xclip)"'
alias -g ANYF='**/*[^~](.)'

# 軟件設置 {{{1
# git {{{2
if [ $IS_ARCH == 0 ];
then
    export GIT_COMMITTER_NAME=bennyye
    export GIT_COMMITTER_EMAIL=bennyye@tencent.com
    export GIT_AUTHOR_NAME=$GIT_COMMITTER_NAME
    export GIT_AUTHOR_EMAIL=$GIT_COMMITTER_EMAIL
fi
# zsh {{{2
# 提示符
# %n --- 用户名
# %~ --- 当前目录
# %h --- 历史记录号
# git 分支显示 {{{3
if (( $+commands[git] )); then
  _nogit_dir=()
  for p in $nogit_dir; do
    [[ -d $p ]] && _nogit_dir+=$(realpath $p)
  done
  unset p

  _setup_current_branch_async () { # {{{4
    typeset -g _current_branch= vcs_info_fd=
    zmodload zsh/zselect 2>/dev/null

    _vcs_update_info () {
      eval $(read -rE -u$1)
      zle -F $1 && vcs_info_fd=
      exec {1}>&-
      # update prompt only when necessary to avoid double first line
      [[ -n $_current_branch ]] && zle reset-prompt
    }

    _set_current_branch () {
      _current_branch=
      [[ -n $vcs_info_fd ]] && zle -F $vcs_info_fd
      cwd=$(pwd -P)
      for p in $_nogit_dir; do
        if [[ $cwd == $p* ]]; then
          return
        fi
      done

      setopt localoptions no_monitor
      coproc {
      _br=$(git branch --no-color 2>/dev/null)
      if [[ $? -eq 0 ]]; then
        _current_branch=$(echo $_br|awk '$1 == "*" {print "("substr($0, 3)")"}')
      fi
      # always gives something for reading, or _vcs_update_info won't be
      # called, fd not closed
      #
      # "typeset -p" won't add "-g", so reprinting prompt (e.g. after status
      # of a bg job is printed) would miss it
      #
      # need to substitute single ' with double ''
      print "typeset -g _current_branch='${_current_branch//''''/''}'"
    }
    disown %{\ _br
    exec {vcs_info_fd}<&p
    # wait 0.1 seconds before showing up to avoid unnecessary double update
    # precmd functions are called *after* prompt is expanded, and we can't call
    # zle reset-prompt outside zle, so turn to zselect
    zselect -r -t 10 $vcs_info_fd 2>/dev/null
    zle -F $vcs_info_fd _vcs_update_info
  }
}

_setup_current_branch_sync () { # {{{4
  _set_current_branch () {
    _current_branch=
    cwd=$(pwd -P)
    for p in $_nogit_dir; do
      if [[ $cwd == $p* ]]; then
        return
      fi
    done

    _br=$(git branch --no-color 2>/dev/null)
    if [[ $? -eq 0 ]]; then
      _current_branch=$(echo $_br|awk '{if($1 == "*"){print "(" substr($0, 3) ")"}}')
    fi
  }
} # }}}

if [[ $_has_re -ne 1 ||
  $ZSH_VERSION =~ '^[0-4]\.' || $ZSH_VERSION =~ '^5\.0\.[0-5]' ]]; then
# zsh 5.0.5 has a CPU 100% bug with zle -F
_setup_current_branch_sync
else
  _setup_current_branch_async
fi
typeset -gaU precmd_functions
precmd_functions+=_set_current_branch
fi
# }}}3
# prompt {{{3
setopt PROMPT_SUBST

E=$'\x1b'

function lambda()
{
  if [[ $? == 0  ]]; then
    echo '\n%F{yellow}λ'
  else
    echo '%F{red}(%?)\nλ'
  fi
}

if (( $+commands[starship] )); then
    eval $(starship init zsh)
else
    ipL=$(ip -o -4 addr | awk -F "inet |/" '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)

    if [[ -n $DISPLAY ]]; then
      PS1='%F{74}%* %F{114}%n%F{white} @ %F{174}%M %F{white}ω %F{142}%~ %F{yellow}$_current_branch$(lambda) %f'
    elif [[ -n $SSH_CONNECTION ]]; then
      PS1='%F{74}%* %F{114}%n%F{white}@%F{174}$ipL%F{white}:%F{142}%~ %F{yellow}$_current_branch$(lambda) %f'
    else
      # do not use unicode in tty
      PS1='%F{yellow}%* %F{cyan}%n%F{white} @ %F{magenta}%M %F{white}in %F{green}%~ %F{red}$_current_branch %F{cyan}
    >>>%f '
    fi

    # 次提示符：使用暗色
    PS2="%{${E}[2m%}%_>%{${E}[0m%} "
    unset E

    # 分割线
    # PS1=$'${(r:$COLUMNS::\u2500:)}'$PS1
fi

CORRECT_IGNORE='_*'
READNULLCMD=less
watch=(notme root)
WATCHFMT='%n has %a %l from %M'
REPORTTIME=5

# ranger-cd {{{2
# Automatically change the directory in bash after closing ranger
#
# This is a bash function for .bashrc to automatically change the directory to
# the last visited one after ranger quits.
# To undo the effect of this function, you can type "cd -" to return to the
# original directory.
function ranger-cd {
  tempfile="$(mktemp)"
  /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
  test -f "$tempfile" &&
    if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
      cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
  }
# Tmux {{{2
export DISABLE_AUTO_TITLE=true
# Ripgrep {{{2
export RIPGREP_CONFIG_PATH=~/.ripgreprc
# Plugin {{{1
source ~/.zsh/plugin/git.zsh
[ $commands[sk] ] && source ~/.zsh/plugin/sk-tools.zsh
source ~/.zsh/plugin/commacd.zsh

source ~/.zsh/plugin/zsh-autosuggestions.zsh

source ~/.zsh/plugin/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.zsh/plugin/autopair.zsh && autopair-init

if [[ $IS_ARCH == 1 ]]; then
    ZSH_AUTOSUGGEST_USE_ASYNC=1
    FAST_HIGHLIGHT[use_async]=1
fi


export _ZL_FZF=sk
source ~/.zsh/plugin/z.lua.plugin.zsh

source ~/.zsh/plugin/docker-alias.zsh

# Modeline {{{1
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local || true

# put in .zshrc.local
#
# export KEYID=0x11CD7ED945B1A60F
#
# export GPG_TTY="$(tty)"
# export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
# gpgconf --launch gpg-agent

# vim:fdm=marker
