# shellcheck disable=SC1090
if exists dircolors; then
  eval "$(dircolors -b ~/.shell/dir_colors)"
fi

if exists zoxide; then
  eval "$(zoxide init bash)"
fi

source ~/.shell/plugins/fzf.sh
source ~/.shell/plugins/git.sh
