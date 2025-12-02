source ~/.shell/alias.sh
alias pl='print -l'
alias ca='noglob calc'
# 後綴別名
alias -s pdf=zathura
alias -s {jpg,png,gif}=feh
alias -s tar="tar -xvf"
alias -s {tgz,gz}="tar -xvzf"
alias -s bz2="tar -xvjf"
alias -s 7z="7zr x"
alias -s zip=unzip
# 全局别名
# 当前目录下最后修改的文件
# 来自 http://roylez.heroku.com/2010/03/06/zsh-recent-file-alias.html
alias -g NN="*(oc[1])"
alias -g NNF="*(oc[1].)"
alias -g NUL="/dev/null"
alias -g XS='"$(xclip)"'
alias -g ANYF='**/*[^~](.)'

# Functions
autoload -U zargs
autoload -U zmv
alias update='exec zsh'

autoload -Uz run-help
autoload -Uz run-help-git
(( ${+aliases[run-help]} )) && unalias run-help
alias help=run-help

mvpc () { mv $1 "`echo $1|ascii2uni -a J`" } # 将以 %HH 表示的文件名改正常
vman () { vim +"set ft=man" +"Man $*" }
nocolor () { sed -r "s:\x1b\[[0-9;]*[mK]::g" }

cmakeG() {
  read -rA args <<<"$CMAKE_FLAGS"
  # shellcheck disable=SC2068
  cmake -B.build -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ${args[@]} "$@" . \
    && ln -sf .build/compile_commands.json .
}

alias CMAKE-CLEAN='rm -rf .ccls-cache build compile_commands.json'
alias ctestR='ctest -j$(nproc) --rerun-failed --output-on-failure'

calc() {
    zmodload zsh/mathfunc
    echo $(($*))
}
# 進制轉換
0x() {
    echo $((16#$1))
}

0o() {
    echo $((8#$1))
}

0b() {
    echo $((2#$1))
}

p16() {
    echo $(([#16] $1))
}

p8() {
    echo $(([#8] $1))
}

p2() {
    echo $(([#2] $1))
}
# 併發
rr() {
    (($+max_process)) || typeset -gi max_process=10
    (($+running_process)) || typeset -gA running_process=()

    while {getopts j:h arg} {
        case $arg {
            (j)
            ((OPTARG > 0)) && max_process=$OPTARG
            ;;

            (h)
            echo "Usage: $0 [-j max_process] [cmd] [args]"
            return
            ;;
        }
    }

    shift $((OPTIND - 1))

    (($# == 0)) && {
        for i (${(k)running_process}) {
            [[ -e $i ]] || unset "running_process[$i]"
        }

        echo "running/max: $#running_process/$max_process"
        (($#running_process > 0)) && echo "pids:" ${${(k)running_process/\/proc\/}/\/exe}
        return 0
    }

    while ((1)) {
        local running_process_num=$#running_process

        if (($running_process_num < max_process)) {
            $* &
            running_process[/proc/$!/exe]=1
            return
        }

        for i (${(k)running_process}) {
            [[ -e $i ]] || unset "running_process[$i]"
        }

        (($#running_process == $running_process_num)) && {
            echo "wait $running_process_num pids:" ${${(k)running_process/\/proc\/}/\/exe}
            inotifywait -q ${(k)running_process}
        }
    }
}

(( ${+aliases[pain]} )) && unalias pain
pain() {
  sudo pacman -S --needed $*
  ZCOMPCACHE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump.$ZSH_VERSION"
  rm -f $ZCOMPCACHE
  rehash
}


# zellij_tab_name_update() {
#   if [[ -n $ZELLIJ ]]; then
#     local current_dir=$PWD
#     if [[ $current_dir == $HOME ]]; then
#       current_dir="~"
#     else
#       current_dir=${current_dir##*/}
#     fi
#     command nohup zellij action rename-tab $current_dir > /dev/null 2>&1
#   fi
# }

# zellij_tab_name_update
# chpwd_functions+=(zellij_tab_name_update)

# _zellij_tab_name_preexec() {
#     local full_command="$1"
#     local -a parts=("${(@s: :)full_command}")
#     local command_name="${parts[1]}"
#     [[ $command_name == (ls|l|cd) ]] || \
#       command nohup zellij action rename-tab $command_name > /dev/null 2>&1
# }
# _zellij_tab_name_precmd() {
#     command nohup zellij action rename-tab zsh > /dev/null 2>&1
# }
# autoload -U add-zsh-hook
# add-zsh-hook preexec _zellij_tab_name_preexec
# add-zsh-hook precmd _zellij_tab_name_precmd

zl-clean() {
  zellij kill-all-sessions -y
  zellij delete-all-sessions -fy
}

