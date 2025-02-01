path_remove() {
  PATH=$(echo -n "$PATH" | awk -v RS=: -v ORS=: "\$0 != \"$1\"" | sed 's/:$//')
}

path_append() {
  path_remove "$1"
  PATH="${PATH:+"$PATH:"}$1"
}

path_prepend() {
  # path_remove "$1"
  PATH="$1${PATH:+":$PATH"}"
}

exists() {
  command -v "$1" >/dev/null 2>&1
}

# print the 16 terminal colors
base16_colors() {
  for i in {0..15}; do
    printf "\e[48;5;${i}m  \e[0m"
    if [ $((($i + 1) % 8)) -eq 0 ]; then
      printf "\n"
    fi
  done
}
