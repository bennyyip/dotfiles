$env.config.buffer_editor = "vim"
$env.config.show_banner = false

$env.config.table.mode = "compact_double"
$env.config.filesize.unit = 'binary'

$env.config.history = {
  file_format: sqlite
  max_size: 1_000_000
  sync_on_enter: true
  isolation: true
}

$env.PROMPT_COMMAND = {||
    let dir = match (do -i { $env.PWD | path relative-to $nu.home-path }) {
        null => $env.PWD
        '' => '~'
        $relative_pwd => ([~ $relative_pwd] | path join)
    }

    let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
    let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
    let path_segment = $"($path_color)($dir)(ansi reset)"

    $path_segment | str replace --all (char path_sep) $"($separator_color)/($path_color)"
}

$env.PROMPT_COMMAND_RIGHT = {||
    let time_segment = (date now | format date "%H:%M:%S")

    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code, (char space), $time_segment] | str join)
}

export-env {
    load-env {
        EDITOR: "vim"
        VISUAL: "vim"
        PAGER: "less"
        LESS: "-FRXM"
    }
}

# alias {{{1
alias l = eza --group-directories-first -blF --icons=auto
alias ll = eza --long
alias vr = gvim-remote
alias ols = python ~/dotfiles/bin/open-livestream.py
# git {{{2
alias g = git
alias gst = git status -sb
alias gss = git stash

alias gci = git commit
alias gcm = git commit -m
alias gca = git commit -a
alias gcf = git commit --fixup
alias gcam = git commit -a -m
alias gcan! = git commit --verbose --all --no-edit --amend

alias gp = git push
alias gpl = git pull --rebase --autostash
alias grv = git remote --verbose
alias gop = git open

alias glg = git log -n 100 --graph --pretty="format:%C(red)%h%Creset %C(yellow)%G?%Creset%C(auto)%d%Creset %s %Cgreen(%cd) %C(bold blue)<%aN>%Creset" --date=relative
alias gll = git log --pretty="format:%C(red)%h%Creset %C(yellow)%G?%Creset%C(auto)%d%Creset %s %Cgreen(%cd) %C(bold blue)<%aN>%Creset" --date=relative
alias gL = gl --stat

def gfk [] {
  let fork_url = (git remote get-url origin | awk -F '/' '{printf "git@github.com:bennyyip/%s", $NF}')
  git remote add fork $fork_url
}
# }}}
# }}}

# commands {{{1
def zl [name?: string] { # {{{2
# Check if zellij exists
  if (which zellij | is-empty) {
    print -e "zellij not exists"
    exit 1
  }

  def _zellij_attach_or_switch [session_name: string] {
    if ($env.ZELLIJ? != null) {
      zellij pipe -p switch -- $"-s ($session_name) --layout compact"
    } else {
      zellij attach -c $session_name
    }
  }

# If session name provided as argument
  if ($name != null) {
    _zellij_attach_or_switch $name
    return
  }

# Get list of active sessions
  let sessions = (
    try {
      zellij list-sessions -n | grep -v EXITED | awk '{print $1}'
    } catch {
      ""
    }
  )

  if ($sessions != "") {
    let selected = (
      if (which fzf | is-empty) == false {
        $sessions | fzf
      } else {
        ""
      }
    )

    if ($selected != "") {
      _zellij_attach_or_switch $selected
    } else {
      _zellij_attach_or_switch "main"
    }
  }
}
# }}}
##@complete external
def --wrapped fuzzy [ # {{{2
    ...args
]: list<any> -> list<any>, table -> table, record -> record, string -> string {
    let input = $in

    match ($input | describe | str replace --regex '<.*' '') {
        "list" => {
            $input | str join "\n" | ^fzf ...$args | lines
        },
        _ => {
            $input | each {|i| $i | to json --raw}
              | str join "\n"
              | ^fzf ...$args
              | lines
              | each {$in | from json}
              | reduce {|it, acc| $acc | append $it}
        }
    }
}
# }}}
def add-magnet [ # {{{2
  type: string@"nu-complete magnet-type"
  magnet?: string
] {
  let QB_API = "http://localhost:8964/api/v2"

  let magnet = (
    if ($magnet == null or $magnet == "") {
      get-clipboard.exe
    } else {
      $magnet
    }
  )

  if not ($magnet | str starts-with "magnet:?xt=") {
    print "invalid magnet url"
    return
  }

  let cat = (
    if ($type == "Normal") {
      ""
    } else {
      $type
    }
  )

  let form = {
    urls: $magnet
    ratioLimit: 1.0
    category: $cat
    autoTMM: "true"
  }

  let resp = (http post $"($QB_API)/torrents/add" --content-type "multipart/form-data" $form)
  print $resp
}

def "nu-complete magnet-type" [] {
  ["A" "Game" "TV" "Movie" "Anime" "PhotoBook" "Normal" "Music"]
}
# }}}

if ($nu.os-info.name == "windows") { # {{{ 1
# MSVC {{{2
let MSVC_BASE = $"($env.HOME)/portable-msvc/msvc/"

# Visual C++ / Windows SDK versions (adjust if needed)
let VCTOOLS_VERSION = "14.51.36231"
let WINDOWS_SDK_VERSION = "10.0.28000.0"

export-env {
  load-env {
    VSCMD_ARG_HOST_ARCH: "x64"
    VSCMD_ARG_TGT_ARCH: "x64"
    VCToolsVersion: $VCTOOLS_VERSION
    WindowsSDKVersion: $WINDOWS_SDK_VERSION
    VCToolsInstallDir: $"($MSVC_BASE)VC/Tools/MSVC/($VCTOOLS_VERSION)/"
    WindowsSdkBinPath: $"($MSVC_BASE)Windows Kits/10/bin/"
    INCLUDE: $"($MSVC_BASE)VC/Tools/MSVC/($VCTOOLS_VERSION)/include;($MSVC_BASE)Windows Kits/10/Include/($WINDOWS_SDK_VERSION)/ucrt;($MSVC_BASE)Windows Kits/10/Include/($WINDOWS_SDK_VERSION)/shared;($MSVC_BASE)Windows Kits/10/Include/($WINDOWS_SDK_VERSION)/um;($MSVC_BASE)Windows Kits/10/Include/($WINDOWS_SDK_VERSION)/winrt;($MSVC_BASE)Windows Kits/10/Include/($WINDOWS_SDK_VERSION)/cppwinrt"
    LIB: $"($MSVC_BASE)VC/Tools/MSVC/($VCTOOLS_VERSION)/lib/x64;($MSVC_BASE)Windows Kits/10/Lib/($WINDOWS_SDK_VERSION)/ucrt/x64;($MSVC_BASE)Windows Kits/10/Lib/($WINDOWS_SDK_VERSION)/um/x64"
  }
}
$env.path ++= [$"($MSVC_BASE)VC/Tools/MSVC/($VCTOOLS_VERSION)/bin/Hostx64/x64", $"($MSVC_BASE)Windows Kits/10/bin/($WINDOWS_SDK_VERSION)/x64", $"($MSVC_BASE)Windows Kits/10/bin/($WINDOWS_SDK_VERSION)/x64/ucrt"]
# }}}
# LLVM {{{2
let MINGW_ARCH = "x86_64-w64-mingw32"
export-env {
  load-env {
    EXE: ".exe"
    MINGW_ARCH: "x86_64-w64-mingw32"
    PKG_CONFIG_PATH: [$"($env.HOME)/sdl/($MINGW_ARCH)/lib/pkgconfig"]
  }
}

$env.PATH ++= [$"($env.HOME)/llvm-mingw/bin", $"($env.HOME)/sdl/($env.MINGW_ARCH)/bin"]
# }}}
}
# }}}

$env.path ++= ["~/bin"]

source $"($nu.default-config-dir)/zoxide.nu"
source $"($nu.default-config-dir)/gruvbox.nu"

# vim:fdm=marker:fdl=0
