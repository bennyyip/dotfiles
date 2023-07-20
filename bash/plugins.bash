# dircolors
if [[ "$(tput colors)" == "256" ]]; then
  export LS_COLORS="$(cat "$HOME/.shell/dircolors-gruvbox-dark")"
else
  if exists dircolors; then
    eval "$(dircolors -b)"
  fi
fi

source ~/.shell/plugins/commacd.sh
