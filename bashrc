# Functions
# shellcheck disable=SC1090
source ~/.shell/bootstrap.sh

# Allow local customizations in the ~/.shell_local_before file
if [ -f ~/.shell_local_before ]; then
  source ~/.shell_local_before
fi

# Allow local customizations in the ~/.bashrc_local_before file
if [ -f ~/.bashrc_local_before ]; then
  source ~/.bashrc_local_before
fi

# Settings
HISTCONTROL=ignoreboth
HISTSIZE=1048576
HISTFILE="$HOME/.bash_history"
SAVEHIST=$HISTSIZE
shopt -s histappend         # append instead of overwrite history
shopt -s autocd             # auto cd when entering a path
shopt -s globstar           # enable "**" wildcard for more subdir
shopt -s checkwinsize       # check window size after each command


# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

# Aliases
source ~/.shell/alias.sh
alias which='(alias; declare -f) | /usr/bin/which --tty-only --read-alias --read-functions --show-tilde --show-dot'

# Bash Functions
update() {
  source ~/.bashrc
}

# Custom prompt
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

machine_name() {
    if [[ -f $HOME/.name ]]; then
        cat $HOME/.name
    else
        cat /etc/hostname
    fi
}

PROMPT_DIRTRIM=3
PS1="${ATTRIBUTE_BOLD}${COLOR_DEFAULT}${COLOR_CYAN}\\u${COLOR_DEFAULT} ${COLOR_WHITE}@${COLOR_DEFAULT} ${COLOR_MAGENTA}$(machine_name)${COLOR_DEFAULT}${COLOR_WHITE} >>= ${COLOR_DEFAULT}${COLOR_GREEN}\w${COLOR_DEFAULT}\n\$(if [ \$? -ne 0 ]; then echo \"${COLOR_RED}!${COLOR_DEFAULT}\"; else echo \"${COLOR_BLUE}\$${COLOR_DEFAULT}\"; fi)${ATTRIBUTE_RESET} "
PS2="${COLOR_BLUE}>${COLOR_DEFAULT} "

COLOR_GRAY='\e[38;5;246m'

demoprompt() {
    PROMPT_DIRTRIM=1
    PS1="${COLOR_GRAY}\w ${COLOR_BLUE}\$ "
    trap '[[ -t 1 ]] && tput sgr0' DEBUG
}

# Plugins
if exists dircolors; then
  eval "$(dircolors -b ~/.shell/dir_colors)"
fi

if exists zoxide; then
  eval "$(zoxide init bash)"
fi

source ~/.shell/plugins/fzf.sh

# Allow local customizations in the ~/.shell_local_after file
if [ -f ~/.shell_local_after ]; then
  source ~/.shell_local_after
fi

# Allow local customizations in the ~/.bashrc_local_after file
if [ -f ~/.bashrc_local_after ]; then
  source ~/.bashrc_local_after
fi

# Allow private customizations (not checked in to version control)
if [ -f ~/.shell_private ]; then
  source ~/.shell_private
fi
