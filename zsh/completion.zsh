# zstyle :compinstall filename "$ZDOTDIR/.zshrc"
fpath+=($ZDOTDIR/Completion)

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
zstyle ':completion:*' cache-path ${XDG_CACHE_HOME:-$HOME/.cache}/zsh

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

# zstyle -e ':completion:*' special-dirs \
#   '[[ $PREFIX == (../)#(|.|..) ]] && reply=(..)'

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

# cd ~ 补全顺序
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'

autoload -Uz compinit
ZCOMPCACHE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump.$ZSH_VERSION"
compinit -u -C -d "$ZCOMPCACHE"

compdef pkill=killall
compdef pgrep=killall

compdef vman=man
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
