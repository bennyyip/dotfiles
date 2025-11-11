export CC=clang
export EXE=".exe"
MINGW_BASE="$HOME/llvm-mingw"
MINGW_ARCH="x86_64-w64-mingw32"
PKG_CONFIG_PATH="$HOME/sdl/$MINGW_ARCH/lib/pkgconfig;$PKG_CONFIG_PATH"
PATH="$MINGW_BASE/bin;$PATH"
PATH="$PATH;$HOME/sdl/$MINGW_ARCH/bin;$MINGW_BASE/x64/mingw/bin;$HOME/.bun/bin"

# PYTHONHOME="$MINGW_BASE/python"
# PATH="$PATH;$PYTHONHOME/bin"
# export PYTHONHOME
# alias python='python3'

CMAKE_GENERATOR="MinGW Makefiles"
CMAKE_PREFIX_PATH="$HOME/opt"
CMAKE_INSTALL_PREFIX="$MINGW_BASE"
export CMAKE_GENERATOR
export CMAKE_PREFIX_PATH
export CMAKE_INSTALL_PREFIX

# -D CMAKE_TOOLCHAIN_FILE=$VCPKG_ROOT/scripts/buildsystems/vcpkg.cmake
export VCPKG_ROOT=$HOME/.local/share/vcpkg

export PATH
export PKG_CONFIG_PATH

export http_proxy=http://192.168.0.1:10808
export https_proxy=http://192.168.0.1:10808

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

source ~/.bashrc

# Custom prompt [[[1
ATTRIBUTE_BOLD='\[\e[1m\]'
ATTRIBUTE_RESET='\[\e[0m\]'
COLOR_DEFAULT='\[\e[39m\]'
COLOR_RED='\[\e[31m\]'
COLOR_GREEN='\[\e[32m\]'
COLOR_YELLOW='\[\e[33m\]'
COLOR_BLUE='\[\e[34m\]'
COLOR_MAGENTA='\[\e[35m\]'
COLOR_CYAN='\[\e[36m\]'
COLOR_WHITE='\[\e[37m\]'


PROMPT_DIRTRIM=3
PS1="${COLOR_CYAN}llvm${COLOR_DEFAULT}${COLOR_WHITE} >>= ${COLOR_DEFAULT}${COLOR_GREEN}\w${COLOR_DEFAULT}\n\$(if [ \$? -ne 0 ]; then echo \"${COLOR_RED}!${COLOR_DEFAULT}\"; else echo \"${COLOR_MAGENTA}\$${COLOR_DEFAULT}\"; fi)${ATTRIBUTE_RESET} "
PS2="${COLOR_BLUE}>${COLOR_DEFAULT} "

COLOR_GRAY='\e[38;5;246m'

printf '\e]2;%s\a' LLVM
# ]]]

alias v="gvim-remote.exe"
alias vr="gvim-remote.exe"
alias ols="~/dotfiles/bin/open-livestream.py"
alias pwsh='pwsh -nol -nop'
# alias ii='pwsh -nol -nop -c ii'
alias ii="'C:\Program Files\Everything 1.5a\Everything64.exe' -filter everything -no-regex -parent"
alias ee="'C:\Program Files\Everything 1.5a\Everything64.exe' -filter everything -no-regex -path"

export AGV_EDITOR='gvim --remote-silent-tab +$line $file'

tsoding() {
  local d="$HOME/Downloads/ytdl/youtube/tsoding"
  yt-dlp --config-locations $HOME/tsoding.conf $@
  fd -e srv3 . "${d}" -x YTSubConverter
  fd -e ass  . "${d}" -x sed -i 's#898.048#960#'
  rm -f "${d}/*.srv3"
}

setbrightness() {
  # https://github.com/newAM/monitorcontrol/
  monitorcontrol --set-luminance $*
}

# zoxide [[[1
# pwd based on the value of _ZO_RESOLVE_SYMLINKS.
__zoxide_pwd() {
    \command pwd -P
}

# cd + custom logic based on the value of _ZO_ECHO.
__zoxide_cd() {
    # shellcheck disable=SC2164
    \command cd "$@"
}

# =============================================================================
#
# Hook configuration for zoxide.
#

# Hook to add new entries to the database.
__zoxide_hook() {
    \command zoxide add -- "$(__zoxide_pwd || true)"
}

# Initialize hook.
if [ "${PS1:=}" = "${PS1#*\$(__zoxide_hook)}" ]; then
    PS1="${PS1}\$(__zoxide_hook)"
fi

# Report common issues.
__zoxide_doctor() {
    [ "${_ZO_DOCTOR:-1}" -eq 0 ] && return 0
    case "${PS1:-}" in
    *__zoxide_hook*) return 0 ;;
    *) ;;
    esac

    _ZO_DOCTOR=0
    \command printf '%s\n' \
        'zoxide: detected a possible configuration issue.' \
        'Please ensure that zoxide is initialized right at the end of your shell configuration file.' \
        '' \
        'If the issue persists, consider filing an issue at:' \
        'https://github.com/ajeetdsouza/zoxide/issues' \
        '' \
        'Disable this message by setting _ZO_DOCTOR=0.' \
        '' >&2
}

# =============================================================================
#
# When using zoxide with --no-cmd, alias these internal functions as desired.
#

# Jump to a directory using only keywords.
__zoxide_z() {
    __zoxide_doctor

    if [ "$#" -eq 0 ]; then
        __zoxide_cd ~
    elif [ "$#" -eq 1 ] && [ "$1" = '-' ]; then
        if [ -n "${OLDPWD}" ]; then
            __zoxide_cd "${OLDPWD}"
        else
            # shellcheck disable=SC2016
            \command printf 'zoxide: $OLDPWD is not set'
            return 1
        fi
    elif [ "$#" -eq 1 ] && [ -d "$1" ]; then
        __zoxide_cd "$1"
    else
        __zoxide_result="$(\command zoxide query --exclude "$(__zoxide_pwd || true)" -- "$@")" &&
            __zoxide_cd "${__zoxide_result}"
    fi
}

# Jump to a directory using interactive search.
__zoxide_zi() {
    __zoxide_doctor
    __zoxide_result="$(\command zoxide query --interactive -- "$@")" && __zoxide_cd "${__zoxide_result}"
}

# =============================================================================
#
# Commands for zoxide. Disable these using --no-cmd.
#

\command unalias z >/dev/null 2>&1 || \true
z() {
    __zoxide_z "$@"
}

\command unalias zi >/dev/null 2>&1 || \true
zi() {
    __zoxide_zi "$@"
}
# ]]]
# vim:fdm=marker:fmr=[[[,]]]:fdl=0:ft=sh
