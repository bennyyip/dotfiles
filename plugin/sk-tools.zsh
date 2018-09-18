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
  local file cmd
  cmd=${1:-vim}
  file=$(tail -n +2 ~/.LfCache/python3/mru/mruCache | \
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

fi
# vim: se ft=zsh:
