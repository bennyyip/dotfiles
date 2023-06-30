export FZF_DEFAULT_OPTS='--bind tab:down,shift-tab:up --layout=reverse'
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden --follow --exclude .git --exclude .vscode --exclude __pycache__'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND

__fzf_run() {
  if [[ $TERM_PROGRAM = 'tmux' ]]; then
    # fzf-tmux $*
    fzf --height $(__calc_height) $*
  else
    fzf --height $(__calc_height) $*
  fi
}

__cursor_pos() {
  local pos
  exec {tty}<>/dev/tty
  echo -n '\e[6n' >&$tty
  read -rsdR pos <&$tty
  exec {tty}>&-
  [[ $pos =~ '([0-9]+);([0-9]+)$' ]]
  print $match[1] $match[2]
}

__calc_height() {
  local pos
  typeset -i height left want
  pos=($(__cursor_pos))
  left=$((LINES - pos[1] - 1))
  want=$((LINES * 0.4))
  if ((left > want)); then
    height=$left
  else
    height=$want
  fi
  height=$((height + 1)) # the prompt line is used too
  print $height
}

fzf-vim-files() {
  local file cmd
  cmd=${1:-vim}

  file=$(__fzf_run --prompt "$cmd> ")
  if [[ -n $file ]]; then
    ${cmd} $file
  else
    return 130
  fi
}

fzf-vim-mru() {
  local file cmd mru_file
  cmd=${1:-vim}

  if [ -f ~/.LfCache/python3/mru/mruCache ]; then
    mru_file=~/.LfCache/python3/mru/mruCache
  else
    mru_file=~/.LfCache/python2/mru/mruCache
  fi

  file=$(cat ${mru_file} | __fzf_run --prompt "$cmd> ")
  if [[ -n $file ]]; then
    ${cmd} $file
  else
    return 130
  fi
}

fzf-search-history() {
  local cmd
  # TODO: preview at the right, multi-line, syntax-highlighted
  cmd=$(history -n 1 | __fzf_run --no-sort --prompt 'cmd> ')
  if [[ -n $cmd ]]; then
    BUFFER=$cmd
    ((CURSOR = $#BUFFER))
    zle redisplay # for syntax highlight
  fi
}

scd() {
  local _path="$(fd --hidden --follow --exclude .git --exclude .vscode --exclude __pycache__ -H -t d | __fzf_run --no-sort --prompt 'fzfcd> ')"
  [ "${_path}" == "" ] || cd $_path
}

v() {
  fzf-vim-mru
}
vfr() {
  fzf-vim-mru
}
vff() {
  fzf-vim-files
}
if [[ -n vv ]]; then
  vvff() {
    fzf-vim-files vv
  }
  vvfr() {
    fzf-vim-mru vv
  }
fi

# tmux
tm() {
  [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
  if [ $1 ]; then
    tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1")
    return
  fi
  session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | __fzf_run --no-sort --prompt 'tmux session> ' --exit-0) && tmux $change -t "$session" || echo "No sessions found."
}

tmpane() {
  local panes current_window current_pane target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | __fzf_run --no-sort --prompt 'tmux pane> ') || return

  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_window}.${target_pane} &&
      tmux select-window -t $target_window
  fi
}

# vim: se ft=zsh:
