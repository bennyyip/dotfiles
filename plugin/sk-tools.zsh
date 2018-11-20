__cursor_pos () {
  local pos
  exec {tty}<>/dev/tty
  echo -n '\e[6n' >&$tty; read -rsdR pos <&$tty
  exec {tty}>&-
  [[ $pos =~ '([0-9]+);([0-9]+)$' ]]
  print $match[1] $match[2]
}

__calc_height () {
  local pos
  typeset -i height left want
  pos=($(__cursor_pos))
  left=$(( LINES - pos[1] ))
  want=$(( LINES * 0.4 ))
  if (( left > want )); then
    height=$left
  else
    height=$want
  fi
  height=$(( height + 1)) # the prompt line is used too
  print $height
}

sk-vim-mru () {
  local file cmd mru_file
  cmd=${1:-vim}

  if [ -f ~/.LfCache/python3/mru/mruCache ]; then
    mru_file=~/.LfCache/python3/mru/mruCache
  else
    mru_file=~/.LfCache/python2/mru/mruCache
  fi

  file=$(cat ${mru_file} |  \
    sk --height $(__calc_height) --reverse -p "$cmd> ")
  if [[ -n $file ]]; then
    ${=cmd} $file
  else
    return 130
  fi
}

sk-search-history () {
  local cmd
  # TODO: preview at the right, multi-line, syntax-highlighted
  cmd=$(history -n 1 | \
    sk --height $(__calc_height) --no-sort -p 'cmd> ')
  if [[ -n $cmd ]]; then
    BUFFER=$cmd
    (( CURSOR = $#BUFFER ))
    zle redisplay # for syntax highlight
  fi
}

if (( $+commands[sk] )); then
  zle -N sk-search-history
  bindkey "\esr" sk-search-history

  v () { sk-vim-mru }
  if (( $+commands[vv] )); then
    vv-mru () { sk-vim-mru vv }
  fi

  if [[ -f /usr/share/skim/completion.zsh ]]; then
    . /usr/share/skim/completion.zsh
  fi

  if [ $commands[fasd] ]; then # check if fasd is installed
    # skip `posix-alias`
    eval "$(fasd --init zsh-hook zsh-ccomp zsh-ccomp-install \
      zsh-wcomp zsh-wcomp-install)"

    fasd_cd () {
      if [ $# -le 1 ]
      then
	fasd "$@"
      else
	local _fasd_ret="$(fasd -e 'printf %s' "$@")"
	[ -z "$_fasd_ret" ] && return
	[ -d "$_fasd_ret" ] && cd "$_fasd_ret" || printf %s\n "$_fasd_ret"
      fi
    }

    # fasd & skim change directory - jump using fasd if given argument, filter output of fasd using skim else
    z() {
      [ $# -gt 0 ] && fasd_cd -d "$*" && return
      local dir
      dir="$(fasd -Rdl "$1" | sk --height $(__calc_height) --no-sort -p 'cd> ' )" && cd "${dir}" || return 1
    }

    o() {
      cmd=${1:-xdg-open}
      local file
      file="$(fasd -Rfl | sk --height $(__calc_height) --no-sort -p 'open> '  )" && ${cmd} "${file}" || return 1
    }
  fi

  tm() {
    [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
    if [ $1 ]; then
      tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1"); return
    fi
    session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | sk --height $(__calc_height) --no-sort -p 'tmux session> ' --exit-0) &&  tmux $change -t "$session" || echo "No sessions found."
  }

  ftpane() {
    local panes current_window current_pane target target_window target_pane
    panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
    current_pane=$(tmux display-message -p '#I:#P')
    current_window=$(tmux display-message -p '#I')

    target=$(echo "$panes" | grep -v "$current_pane" | sk --height $(__calc_height) --no-sort -p 'tmux pane> ') || return

    target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
    target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

    if [[ $current_window -eq $target_window ]]; then
      tmux select-pane -t ${target_window}.${target_pane}
    else
      tmux select-pane -t ${target_window}.${target_pane} &&
	tmux select-window -t $target_window
    fi
  }

fi
# vim: se ft=zsh:
