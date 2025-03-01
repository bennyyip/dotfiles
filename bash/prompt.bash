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
