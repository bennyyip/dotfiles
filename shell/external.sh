# Tmux
export DISABLE_AUTO_TITLE=true
# Ripgrep
export RIPGREP_CONFIG_PATH=~/.ripgreprc
# rust lsp
export RUST_SRC_PATH=~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/

export EDITOR=vim
export SYSTEMD_EDITOR=vim
export SUDO_EDITOR=vim

# https://emacs.stackexchange.com/questions/77082/how-to-enable-24-bit-color-emacs-on-terminal-when-opening-it-from-a-remote-machi
export COLORTERM=truecolor

export LESS="-FRXM"
# default has -S
export SYSTEMD_LESS="${LESS#-}K"

if exists bat; then
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
  export MANROFFOPT='-c'
fi

if [[ -n $DISPLAY && -z $SSH_CONNECTION ]]; then
  export BROWSER=firefox
  export AGV_EDITOR='vv ''$file:$line:$col'''
else
  export BROWSER=echo
  export AGV_EDITOR='vim +"call setpos(\".\", [0, $line, $col, 0])" ''$file'''
fi
