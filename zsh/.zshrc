# zmodload zsh/zprof
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

source ~/.shell/bootstrap.sh

# Don't call compinit too early. I'll do it myself, at the right time.
export ZGEN_AUTOLOAD_COMPINIT=0

# Allow local customizations in the ~/.shell.local_before file
if [ -f ~/.shell.local_before ]; then
  source ~/.shell.local_before
fi

# Allow local customizations in the ~/.zshrc.local_before file
if [ -f ~/.zshrc.local_before ]; then
  source ~/.zshrc.local_before
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
HISTFILE=$HOME/.histfile
HISTSIZE=100000   # Max events to store in internal history.
SAVEHIST=100000   # Max events to store in history file.
setopt BANG_HIST                 # History expansions on '!'
setopt EXTENDED_HISTORY          # Include start time in history records
setopt APPEND_HISTORY            # Appends history to history file on exit
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
unsetopt SHARE_HISTORY             # Share history between all sessions.
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
setopt AUTO_CD            # Implicit CD slows down plugins
setopt AUTO_PUSHD           # cd -<tab>
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
unsetopt PUSHD_TO_HOME      # Don't push to $HOME when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt MULTIOS              # Write to multiple descriptors.
unsetopt GLOB_DOTS
unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list
#}}}
fpath+=($ZDOTDIR/functions)
source $ZDOTDIR/alias.zsh
source $ZDOTDIR/completion.zsh
source $ZDOTDIR/keymap.zsh
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [ ! -f "$SSH_AUTH_SOCK" ]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi
# Plugins {{{1
if [ $+commands[fzf] ]; then
    source ~/.shell/plugins/fzf.sh
fi

source $ZDOTDIR/plugins/autopair.zsh && autopair-init

# https://github.com/lilydjwg/atuin
# https://blog.lilydjwg.me/2024/1/13/atuin.216770.html
_plugin=$ZDOTDIR/plugins/atuin.zsh
if (( $+commands[atuin] )) && [[ -f $_plugin ]]; then
  . $_plugin
fi

export ZSH_AUTOSUGGEST_MANUAL_REBIND=1
ZSH_AUTOSUGGEST_STRATEGY=(history)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

ZSH_AUTOSUGGEST_USE_ASYNC=1

source $ZDOTDIR/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
FAST_HIGHLIGHT[use_async]=1

source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
HISTORY_SUBSTRING_SEARCH_PREFIXED=1
HISTORY_SUBSTRING_SEARCH_FUZZY=1
# Up arrow:
bindkey '\e[A' history-substring-search-up
bindkey '\eOA' history-substring-search-up
# Down arrow:
bindkey '\e[B' history-substring-search-down
bindkey '\eOB' history-substring-search-down

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

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f $ZDOTDIR/.p10k.zsh ]] || source $ZDOTDIR/.p10k.zsh
# ensure cursor shape
precmd_functions+=(fix_cursor)
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
# }}}
# zprof | head -n 10
# vim:fdm=marker
