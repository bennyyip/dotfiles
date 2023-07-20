# 提示符
# %n --- 用户名
# %~ --- 当前目录
# %h --- 历史记录号
setopt PROMPT_SUBST


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
E=$'\x1b'

function lambda()
{
  if [[ $? == 0  ]]; then
    echo '\n%F{yellow}λ'
  else
    echo '%F{red}(%?)\nλ'
  fi
}

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

# 分割线
# PS1=$'${(r:$COLUMNS::\u2500:)}'$PS1
