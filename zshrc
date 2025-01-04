# Functions
# zmodload zsh/zprof

source ~/.shell/functions.sh

# Bootstrap
source ~/.shell/bootstrap.sh

# External settings
source ~/.shell/external.sh

# Allow local customizations in the ~/.shell.local_before file
if [ -f ~/.shell.local_before ]; then
  source ~/.shell.local_before
fi

# Allow local customizations in the ~/.zshrc.local_before file
if [ -f ~/.zshrc.local_before ]; then
  source ~/.zshrc.local_before
fi

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
fpath=($_zdir/.zsh/Completion $_zdir/.zsh/functions $fpath)
autoload -Uz compinit
compinit
# 變量設置 {{{1
export -U PATH
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
    [[ $TERM != *color* ]] && [[ $TERM != dumb ]] && export TERM=${TERM%%[.-]*}-256color
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
  export LANG=zh_CN.UTF-8
  # This term is quirk. ls doesn't like it.
  # _256colors=1
fi
eval $(dircolors ~/.shell/dir_colors)
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

# maybe autoload zkbd
bindkey  "^[[3~"  delete-char

# ^X^e 用$EDITOR编辑命令
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

zle -C complete-file menu-expand-or-complete _generic
zstyle ':completion:complete-file:*' completer _files

# https://archive.zhimingwang.org/blog/2015-09-21-zsh-51-and-bracketed-paste.html
autoload -Uz bracketed-paste-url-magic
zle -N bracketed-paste bracketed-paste-url-magic

# zsh 5.1+ uses bracketed-paste-url-magic
if [[ $ZSH_VERSION =~ '^[0-4]\.' || $ZSH_VERSION =~ '^5\.0\.[0-9]' ]]; then
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
fi

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

# C-x p 在当前/上一条命令前插入 proxychains -q
autoload -Uz prefix-proxy
zle -N prefix-proxy
bindkey "^Xp" prefix-proxy

zmodload zsh/complist
bindkey -M menuselect '^O' accept-and-infer-next-history
bindkey "^Xo" accept-and-infer-next-history

bindkey "^X^I" complete-file
bindkey "^X^f" complete-file
bindkey "^U" backward-kill-line
bindkey "^]" vi-find-next-char
bindkey "\e]" vi-find-prev-char
bindkey "\eq" push-line-or-edit
bindkey -s "\e[Z" "^P"
bindkey '^Xa' _expand_alias
# bindkey '^[/' _history-complete-older
bindkey '\e ' set-mark-command
bindkey '^[w' kill-region
# 用单引号引起最后一个单词
bindkey -s "^['" "^[] ^f^@^e^[\""
# 打开 zsh 的PDF格式文档
# bindkey -s "^X^D" "evince /usr/share/doc/zsh/zsh.pdf &^M"
bindkey -s "^Xc" "tmux attach -d^M"

bindkey '^[p' up-line-or-search
bindkey '^[n' down-line-or-search
# jump to a position in a command line {{{2
# https://github.com/scfrazer/zsh-jump-target
autoload -Uz jump-target
zle -N jump-target
bindkey "\ej" jump-target

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
bindkey "\eD" zsh-kill-word
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
# 插入当前的所有补全 https://www.zsh.org/mla/workers/2020/msg01232.html {{{2
zstyle ':completion:all-matches::::' completer _all_matches _complete
zstyle ':completion:all-matches:*' old-matches true
zstyle ':completion:all-matches:*' insert true
zstyle ':completion:all-matches:*' file-patterns '%p:globbed-files' '*(-/):directories' '*:all-files'
zle -C all-matches complete-word _generic
bindkey '^Xi' all-matches
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
mvpc () { mv $1 "`echo $1|ascii2uni -a J`" } # 将以 %HH 表示的文件名改正常
vman () { vim +"set ft=man" +"Man $*" }
nocolor () { sed -r "s:\x1b\[[0-9;]*[mK]::g" }

cmakeG() {
  read -rA args <<<"$CMAKE_FLAGS"
  # shellcheck disable=SC2068
  cmake -Bbuild -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ${args[@]} "$@" . \
    && ln -sf build/compile_commands.json .
}

alias CMAKE-CLEAN='rm -rf .ccls-cache build compile_commands.json'
alias ctestR='ctest -j$(nproc) --rerun-failed --output-on-failure'
# 別名 {{{1
source ~/.shell/alias.sh

# 後綴別名 {{{2
alias -s pdf=zathura
alias -s {jpg,png,gif}=feh
alias -s tar="tar -xvf"
alias -s {tgz,gz}="tar -xvzf"
alias -s bz2="tar -xvjf"
alias -s 7z="7zr x"
alias -s zip=unzip

# 全局别名 {{{2
# 当前目录下最后修改的文件
# 来自 http://roylez.heroku.com/2010/03/06/zsh-recent-file-alias.html
alias -g NN="*(oc[1])"
alias -g NNF="*(oc[1].)"
alias -g NUL="/dev/null"
alias -g XS='"$(xclip)"'
alias -g ANYF='**/*[^~](.)'
# Prompt {{{1
if [[ $TERM != dumb && -f $(which starship 2>/dev/null) ]]; then
    eval "$(starship init zsh)"
else
    source ~/.zsh/prompt.zsh
fi
# ensure cursor shape
_fix_cursor() {
   echo -ne '\e[6 q'
}
precmd_functions+=(_fix_cursor)
# Plugin {{{1
source ~/.zsh/plugins/git.zsh
alias glg=glods
if [ $commands[fzf] ]; then
    source ~/.shell/plugins/fzf.sh
    zle -N fzf-search-history
    bindkey "\esa" fzf-search-history
    bindkey "\es\ea" fzf-search-history
    export _ZL_FZF=fzf
fi
source ~/.shell/plugins/commacd.sh

source ~/.zsh/plugins/zsh-autosuggestions.zsh

source ~/.zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.zsh/plugins/autopair.zsh && autopair-init

# https://github.com/lilydjwg/atuin
# https://blog.lilydjwg.me/2024/1/13/atuin.216770.html
_plugin=${_zdir}/.zsh/plugins/atuin.zsh
if (( $+commands[atuin] )) && [[ -f $_plugin ]]; then
  . $_plugin
fi

# if [[ $IS_ARCH == 1 ]]; then
ZSH_AUTOSUGGEST_USE_ASYNC=1
FAST_HIGHLIGHT[use_async]=1
# fi

if (( $+commands[zoxide] )) && [[ ! -f ~/.local/share/zoxide/db.zo || $(zstat +uid ~/.local/share/zoxide/db.zo) == $UID ]]; then
  eval "$(zoxide init zsh)"
  function z () {
    if [[ "$#" -eq 0 ]]; then
      __zoxide_z ''
    else
      __zoxide_z "$@"
    fi
  }
  export _ZO_RESOLVE_SYMLINKS=1
  alias zf=zi
fi
# if zoxide loads but the directory is readonly, remove the chpwd hook
if [[ ${chpwd_functions[(i)__zoxide_hook]} -le ${#chpwd_functions} && \
  -d ~/.local/share/zoxide && ! -w ~/.local/share/zoxide ]]; then
  chpwd_functions[(i)__zoxide_hook]=()
fi

zb() {
  _commacd_backward >/dev/null 2>&1
}

zbi() {
  zb
  scd
}

source ~/.zsh/plugins/docker-alias.zsh

# Modeline {{{1
# Allow local customizations in the ~/.shell.local_after file
if [ -f ~/.shell.local_after ]; then
  source ~/.shell.local_after
fi

# Allow local customizations in the ~/.zshrc.local_after file
if [ -f ~/.zshrc.local_after ]; then
  source ~/.zshrc.local_after
fi

# Allow private customizations (not checked in to version control)
if [ -f ~/.shell_private ]; then
  source ~/.shell_private
fi

# zprof
# vim:fdm=marker
