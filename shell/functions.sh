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

zb() {
  gitroot=$(git rev-parse --show-toplevel 2>/dev/null)
  if [[ -n $gitroot && "$PWD" != "$gitroot" ]]; then
    builtin cd "${gitroot}"
    pwd
  fi
}
