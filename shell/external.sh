# Tmux
export DISABLE_AUTO_TITLE=true
# Ripgrep
export RIPGREP_CONFIG_PATH=~/.ripgreprc
# rust lsp
export RUST_SRC_PATH=~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/

export EDITOR=vim
export SYSTEMD_EDITOR=vim
export SUDO_EDITOR=vim

# https://emacs.stackexchange.com/questions/77082/how-to-enable-24-bit-color-emacs-on-terminal-when-opening-it-from-a-remote-machi
export COLORTERM=truecolor

export LESS="-FRXM"
# default has -S
export SYSTEMD_LESS="${LESS#-}K"

# if exists bat; then
#   export MANPAGER="sh -c 'col -bx | bat -l man -p'"
#   export MANROFFOPT='-c'
# fi

# 让 less 将粗体/下划线等显示为彩色
export LESS_TERMCAP_mb=$'\x1b[91m'
export LESS_TERMCAP_md=$'\x1b[38;5;74m'
export LESS_TERMCAP_me=$'\x1b[0m'
export LESS_TERMCAP_se=$'\x1b[0m'
export LESS_TERMCAP_so=$'\x1b[7m'
export LESS_TERMCAP_ue=$'\x1b[0m'
export LESS_TERMCAP_us=$'\x1b[04;38;5;146m'
# man 手册支持彩色
export GROFF_NO_SGR=1


if [[ -n $DISPLAY && -z $SSH_CONNECTION ]]; then
  export BROWSER=firefox
  export AGV_EDITOR='vv ''$file:$line:$col'''
else
  export BROWSER=echo
  export AGV_EDITOR='vim +"call setpos(\".\", [0, $line, $col, 0])" ''$file'''
fi

sanenv() {
    local common=abort_on_error=1:halt_on_error=1:allocator_may_return_null=1
    ASAN_OPTIONS=$common
    ASAN_OPTIONS+=:print_legend=0
    ASAN_OPTIONS+=:detect_leaks=0
    ASAN_OPTIONS+=:detect_stack_use_after_return=1
    ASAN_OPTIONS+=:max_malloc_fill_size=$((1<<30))
    UBSAN_OPTIONS=$common
    TSAN_OPTIONS=$common
    export ASAN_OPTIONS UBSAN_OPTIONS TSAN_OPTIONS
}

sanenv
# vim:ft=bash
