HISTCONTROL=ignoreboth
HISTSIZE=1048576
HISTFILE="$HOME/.bash_history"
SAVEHIST=$HISTSIZE
shopt -s histappend         # append instead of overwrite history
shopt -s autocd             # auto cd when entering a path
shopt -s globstar           # enable "**" wildcard for more subdir
shopt -s checkwinsize       # check window size after each command
