# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# zmodload zsh/zprof

source ~/.shell/functions.sh
source ~/.shell/bootstrap.sh # Bootstrap
source ~/.shell/external.sh # External settings

# Allow local customizations in the ~/.shell.local_before file
if [ -f ~/.shell.local_before ]; then
  source ~/.shell.local_before
fi

# Allow local customizations in the ~/.zshrc.local_before file
if [ -f ~/.zshrc.local_before ]; then
  source ~/.zshrc.local_before
fi

_zdir=${ZDOTDIR:-$HOME}

ZSH_CACHE_DIR=$_zdir/.ZSH_CACHE_DIR
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

export -U PATH
# SETOPT {{{1
unsetopt BEEP               # Hush now, quiet now.
setopt rm_star_silent       # rm * 时不要提示
setopt interactive_comments # 允许在交互模式中使用注释
setopt auto_continue        # disown 后自动继续进程
setopt ksh_option_print

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>' # stop at /
setopt rc_quotes # 单引号中的 '' 表示一个 ' （如同 Vimscript 中者）
unsetopt nomatch

## Jobs
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt NOTIFY             # Report status of background jobs immediately.
unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt HUP              # Don't kill jobs on shell exit.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.
# History
HISTFILE=${_zdir}/.histfile
HISTSIZE=100000   # Max events to store in internal history.
SAVEHIST=100000   # Max events to store in history file.
setopt BANG_HIST                 # History expansions on '!'
setopt EXTENDED_HISTORY          # Include start time in history records
setopt APPEND_HISTORY            # Appends history to history file on exit
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Remove old events if new event is a duplicate
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_REDUCE_BLANKS        # Minimize unnecessary whitespace
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing non-existent history.
setopt hist_fcntl_lock 2>/dev/null
## Directories
DIRSTACKSIZE=9
unsetopt AUTO_CD            # Implicit CD slows down plugins
setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
unsetopt PUSHD_TO_HOME      # Don't push to $HOME when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt MULTIOS              # Write to multiple descriptors.
unsetopt GLOB_DOTS
unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list
# Completion {{{1
zstyle :compinstall filename "$_zdir/.zshrc"
fpath=($_zdir/.zsh/Completion $_zdir/.zsh/functions $fpath)

setopt listpacked         # 补全列表不同列可以使用不同的列宽
setopt magic_equal_subst  # 补全 identifier=path 形式的参数
setopt COMPLETE_IN_WORD   # Complete from both ends of a word.
setopt EXTENDED_GLOB      # Use extended globbing syntax.
setopt PATH_DIRS          # Perform path search even on command names with slashes.
setopt AUTO_MENU          # Show completion menu on a successive tab press.
setopt AUTO_LIST          # Automatically list choices on ambiguous completion.
# setopt AUTO_PARAM_SLASH # If completed parameter is a directory, add a trailing slash.
# setopt AUTO_PARAM_KEYS
unsetopt FLOW_CONTROL     # Redundant with tmux
# unsetopt MENU_COMPLETE  # Do not autoselect the first completion entry.
unsetopt COMPLETE_ALIASES # Disabling this enables completion for aliases
# unsetopt ALWAYS_TO_END  # Move cursor to the end of a completed word.
unsetopt CASE_GLOB
unsetopt CORRECT_ALL

# Completion is slow. Use a cache! For the love of god, use a cache...
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh"

# Expand partial paths, e.g. cd f/b/z == cd foo/bar/baz (assuming no ambiguity)
zstyle ':completion:*:paths' path-completion yes

# Fix slow one-by-one character pasting when bracketed-paste-magic is on. See
# zsh-users/zsh-syntax-highlighting#295
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

eval $(dircolors ~/.shell/dir_colors)
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Fuzzy match mistyped completions.
zstyle ':completion:*' completer _complete _match _approximate _list
zstyle ':completion:*' matcher-list 'm:{[:lower:]-}={[:upper:]_}' 'r:[[:ascii:]]||[[:ascii:]]=** r:|?=**'
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
# Increase the number of errors based on the length of the typed word.
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
# Don't complete unavailable commands.
zstyle ':completion:*:(functions|parameters)' ignored-patterns '(_*|.*|-*|+*|autosuggest-*|pre(cmd|exec))'
# Group matches and describe.
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format $'\e[2m -- %d --\e[0m'
zstyle ':completion:*:corrections' format $'\e[01;33m -- %d (errors: %e) --\e[0m'
zstyle ':completion:*:messages' format '%B%F{yellow}%d%f%b'
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
zstyle ':completion:*:errors' format '%B%F{red}No such %d%f%b'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
# Omit parent and current directories from completion results when they are
# already named in the input.
zstyle ':completion:*:*:cd:*' ignore-parents parent pwd
# Merge multiple, consecutive slashes in paths
zstyle ':completion:*' squeeze-slashes true
# Exclude internal/fake envvars
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}
# Sory array completion candidates
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
# Complete hostnames from ssh files too
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.{config/,}ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${${${${(@M)${(f)"$(cat ~/.{config/,}ssh/config{,.local} 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'
# Don't complete uninteresting users
zstyle ':completion:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
  dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
  hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
  mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody nobody 'nixbld*' nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm shutdown squid sshd sync 'systemd-*' uucp vcsa xfs '_*'
# ... unless we really want to.
zstyle '*' single-ignored show
# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'
# PID completion for kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $LOGNAME -o pid,user,command -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# complete manual by their section, from grml
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true

zstyle ':completion:*' menu select
# 在最后尝试使用文件名
zstyle ':completion:*' completer _complete _match _approximate _expand_alias _ignored _files
zstyle -e ':completion:*' special-dirs \
  '[[ $PREFIX == (../)#(|.|..) ]] && reply=(..)'

# complete user-commands for git-*
# https://pbrisbin.com/posts/deleting_git_tags_with_style/
zstyle ':completion:*:*:git:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

autoload -Uz compinit
compinit

compdef pkill=killall
compdef pgrep=killall

compdef g=git
compdef mkcd=mkdir
compdef proxychains=command
compdef watch=command
compdef rlwrap=command
compdef ptyless=command
compdef grc=command
compdef agg=ag 2>/dev/null
compdef rgg=rg 2>/dev/null
compdef downgrade=pactree 2>/dev/null
# bindkey and zle{{{1
bindkey -e

# maybe autoload zkbd
bindkey  "^[[3~"  delete-char
bindkey  '^[[3;3~' backward-delete-word # alt+delete

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
# Functions {{{1
autoload -U zargs
autoload -U zmv
TRAPTERM () { exit }
update () { . $_zdir/.zshrc }
mvpc () { mv $1 "`echo $1|ascii2uni -a J`" } # 将以 %HH 表示的文件名改正常
vman () { vim +"set ft=man" +"Man $*" }; compdef vman=man
nocolor () { sed -r "s:\x1b\[[0-9;]*[mK]::g" }

cmakeG() {
  read -rA args <<<"$CMAKE_FLAGS"
  # shellcheck disable=SC2068
  cmake -B.build -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ${args[@]} "$@" . \
    && ln -sf .build/compile_commands.json .
}

alias CMAKE-CLEAN='rm -rf .ccls-cache build compile_commands.json'
alias ctestR='ctest -j$(nproc) --rerun-failed --output-on-failure'
# Aliases {{{1
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
# Plugins {{{1
if [ $+commands[fzf] ]; then
    source ~/.shell/plugins/fzf.sh
fi
source ~/.zsh/plugins/zsh-autosuggestions.zsh

source ~/.zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.zsh/plugins/autopair.zsh && autopair-init

# https://github.com/lilydjwg/atuin
# https://blog.lilydjwg.me/2024/1/13/atuin.216770.html
_plugin=${_zdir}/.zsh/plugins/atuin.zsh
if (( $+commands[atuin] )) && [[ -f $_plugin ]]; then
  . $_plugin
fi

ZSH_AUTOSUGGEST_USE_ASYNC=1
FAST_HIGHLIGHT[use_async]=1

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

source ~/.zsh/plugins/docker-alias.zsh
# Local config {{{1
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
# zprof | head -n 10
# }}}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.zsh/.p10k.zsh ]] || source ~/.zsh/.p10k.zsh
# ensure cursor shape
_fix_cursor() {
   echo -ne '\e[6 q'
}
precmd_functions+=(_fix_cursor)

# vim:fdm=marker
