# Tmux
export DISABLE_AUTO_TITLE=true
# Ripgrep
export RIPGREP_CONFIG_PATH=~/.ripgreprc
# rust lsp
export RUST_SRC_PATH=~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/

export EDITOR=vim

# https://emacs.stackexchange.com/questions/77082/how-to-enable-24-bit-color-emacs-on-terminal-when-opening-it-from-a-remote-machi
export COLORTERM=truecolor

ya() {
  tmp="$(mktemp -t "yazi-cwd.XXXXX")"
  yazi --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}
