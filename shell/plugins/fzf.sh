export FZF_DEFAULT_OPTS='--bind tab:down,shift-tab:up --layout=reverse'
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=bg+:#3c3836,bg:#1d2021,spinner:#fb4934,hl:#928374,fg:#ebdbb2,header:#928374,info:#8ec07c,pointer:#fb4934,marker:#fb4934,fg+:#ebdbb2,prompt:#fb4934,hl+:#fb4934"
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden --follow --exclude .git --exclude .vscode --exclude __pycache__'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND

if [[ $0 =~ 'bash' ]]; then
  source ~/.shell/plugins/fzf-key-bindings.bash
  bind '"\C-r":reverse-search-history'
  bind -m emacs-standard -x '"\es": __fzf_history__'

  __fzfcmd() {
    echo fzf --height -40% $*
  }
else
  source ~/.shell/plugins/fzf-key-bindings.zsh
  bindkey -M emacs '^R' history-incremental-search-backward
  bindkey "\esa" fzf-history-widget
  bindkey "\es\ea" fzf-history-widget

  __fzfcmd() {
    echo fzf --height $(__calc_height)
  }
fi

__cursor_pos() {
  local pos
  exec {tty}<> /dev/tty
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

  file=$($(__fzfcmd) --prompt "$cmd> ")
  if [[ -n $file ]]; then
    ${cmd} $file
  else
    return 130
  fi
}

fzf-vim-mru() {
  local file cmd mru_file
  cmd=${1:-vim}

  file=$(cat $HOME/.vim_mru_files | grep -v '^#' | $(__fzfcmd) --no-sort --tiebreak=end --prompt "$cmd> ")

  # mru_file=~/.cache/LeaderF/python3/mru/frecency
  # file=$(cat ${mru_file} | awk '$1=""; $2=""; gsub (" ", "", $0);' | $(__fzfcmd) --tiebreak=end --prompt "$cmd> ")
  if [[ -n $file ]]; then
    ${cmd} $file
  else
    return 130
  fi
}

fzf-rg() {
  # Two-phase filtering with Ripgrep and fzf
  #
  # 1. Search for text in files using Ripgrep
  # 2. Interactively restart Ripgrep with reload action
  #    * Press ctrl-r to switch to fzf-only filtering
  # 3. Open the file in Vim
  RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case "
  INITIAL_QUERY="${*:-}"
  : | fzf --ansi --disabled --query "$INITIAL_QUERY" \
    --bind "start:reload:$RG_PREFIX {q}" \
    --bind "change:reload:sleep 0.1; $RG_PREFIX {q} || true" \
    --bind "ctrl-r:unbind(change,ctrl-r)+change-prompt(2. fzf> )+enable-search+clear-query" \
    --color "hl:-1:underline,hl+:-1:underline:reverse" \
    --prompt '1. ripgrep> ' \
    --delimiter : \
    --preview 'bat --color=always {1} --highlight-line {2}' \
    --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
    --bind 'enter:become(vim {1} +{2})'
}

fzf-kill() {
  ps -ef |
    fzf --bind='ctrl-r:reload(date; ps -ef)' \
      --header=$'Press CTRL-R to reload\n\n' --header-lines=2 \
      --preview='echo {}' --preview-window=down,3,wrap \
      --layout=reverse --height=80% | awk '{print $2}' | xargs kill -9
}
scd() {
  local _path="$(fd -H -L -E .git -E .vscode -E __pycache__ -t d $@ | $(__fzfcmd) -1 --no-sort --prompt 'cd> ')"
  [ "${_path}" == "" ] || cd $_path
}
zbi() {
  zb
  scd
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
  [[ -n $TMUX ]] && change="switch-client" || change="attach-session"
  if [ $1 ]; then
    tmux $change -t "$1" 2> /dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1")
    return
  fi
  session=$(tmux list-sessions -F "#{session_name}" 2> /dev/null | $(__fzfcmd) --no-sort --prompt 'tmux_session> ' --exit-0) && tmux $change -t "$session" || echo "No sessions found."
}

tmpane() {
  local panes current_window current_pane target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | $(__fzfcmd) --no-sort --prompt 'tmux_pane> ') || return

  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_window}.${target_pane} &&
      tmux select-window -t $target_window
  fi
}

# vim: ft=zsh:sw=2
