# shellcheck disable=SC1090
if exists dircolors; then
  eval "$(dircolors ~/.shell/dir_colors)"
fi

if exists zoxide; then
  eval "$(zoxide init bash)"
fi

source ~/.shell/plugins/commacd.sh
source ~/.shell/plugins/fzf.sh
source ~/.shell/plugins/git.sh
