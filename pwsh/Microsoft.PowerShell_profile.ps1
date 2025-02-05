$scriptDir = Split-Path -PaTh $MyInvocation.MyCommand.Definition -Parent
$env:PAGER = 'less.exe -RFXM'

$env:EDITOR = "vim"
$env:PATH =  $env:PATH + ";$HOME/bin"

$env:AGV_EDITOR = 'gvim --remote-silent-tab +$line $file'

$proxy = "http://127.0.0.1:10809"

###############################################################################

import-module $scriptDir\prompt.psm1
function prompt {
  gitFancyPrompt
}

if (Get-Command "zoxide.exe" -ErrorAction SilentlyContinue) {
  Invoke-Expression (& { (zoxide init powershell | % {$_.replace("Set-Location", "Set-LocationEx")} | Out-String) })
}

function zb {
    $root = $(git rev-parse --show-toplevel 2>$null)
    if ($root -eq $null) {
        $root = '..'
    }
    Set-LocationEx $root
}

function zbi {
    $root = $(git rev-parse --show-toplevel 2>$null)
    if ($root -eq $null) {
        $root = '..'
    }
    Set-LocationEx $root
    scd
}

###############################################################################

Import-Module PSReadLine
Set-PSReadLineOption -EditMode vi

Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineOption -HistorySaveStyle SaveIncrementally
Set-PSReadLineOption -MaximumHistoryCount 4000
Set-PSReadLineOption -HistoryNoDuplicates
# history substring search
Set-PSReadlineKeyHandler -Key   UpArrow         -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key   DownArrow       -Function HistorySearchForward

# Tab completion
Set-PSReadlineKeyHandler -Chord 'Shift+Tab'     -Function Complete
Set-PSReadlineKeyHandler -Key   Tab             -Function MenuComplete


Set-PSReadLineKeyHandler -Key   Alt+Backspace   -Function BackwardKillWord
Set-PSReadLineKeyHandler -Key   Alt+b           -Function BackwardWord
Set-PSReadLineKeyHandler -Key   Alt+d           -Function KillWord
Set-PSReadLineKeyHandler -Key   Alt+f           -Function ForwardWord
Set-PSReadLineKeyHandler -Key   Ctrl+a          -Function BeginningOfLine
Set-PSReadLineKeyHandler -Key   Ctrl+b          -Function BackwardChar
Set-PSReadLineKeyHandler -Key   Ctrl+d          -Function DeleteCharOrExit
Set-PSReadLineKeyHandler -Key   Ctrl+e          -Function EndOfLine
Set-PSReadLineKeyHandler -Key   Ctrl+f          -Function ForwardChar
Set-PSReadLineKeyHandler -Key   Ctrl+g          -Function Abort
Set-PSReadLineKeyHandler -Key   Ctrl+n          -Function NextHistory
Set-PSReadLineKeyHandler -Key   Ctrl+p          -Function PreviousHistory
Set-PSReadLineKeyHandler -Key   Ctrl+w          -Function BackwardKillWord
Set-PSReadlineKeyHandler -Chord 'Ctrl+x,Ctrl+e' -Function ViEditVisually
Set-PSReadlineKeyHandler -Key   Ctrl+Backspace  -Function UnixWordRubout

Set-PSReadlineKeyHandler -Chord 'Ctrl+v' -ScriptBlock {
  $clipboard = Get-Clipboard -Raw
  if ($clipboard -match '^\s*(http|ftp|magnet)' -or `
    ($clipboard.trim().StartsWith("C:\")) -or `
    ($clipboard.trim().StartsWith("C:/"))) {
    $clipboard = $clipboard.Trim()
    $clipboard = "`"${clipboard}`""
  }
  [Microsoft.PowerShell.PSConsoleReadLine]::Insert($clipboard)
}

###############################################################################

Import-Module cd-extras

###############################################################################

# Import-Module "$scriptDir\venvlink-autoenv.psm1"

###############################################################################

if (Test-Path "$env:USERPROFILE\local.psm1") {
  Import-Module "$env:USERPROFILE\local.psm1"
}

###############################################################################

# https://github.com/junegunn/fzf/releases
# https://github.com/BurntSushi/ripgrep/releases
# https://github.com/sharkdp/fd/releases
# https://github.com/sharkdp/bat/releases

if (Get-Command "fzf.exe" -ErrorAction SilentlyContinue) {
  Import-Module PSFzf
  $env:FZF_DEFAULT_OPTS = '--bind tab:down,shift-tab:up --layout=reverse'
  $env:FZF_DEFAULT_COMMAND = 'fd --type f --strip-cwd-prefix --hidden --follow --exclude .git --exclude .vscode --exclude __pycache__'
  $env:FZF_CTRL_T_COMMAND = $env:FZF_DEFAULT_COMMAND
  Set-Alias frg Invoke-PsFzfRipgrep
  Set-Alias fkill Invoke-FuzzyKillProcess
  Set-Alias fgs Invoke-FuzzyGitStatus
  Set-PsFzfOption -PSReadlineChordReverseHistory 'Alt+s' -PSReadlineChordProvider 'Ctrl+t'
  function scd { $result = $null; fd -E .git -E __pycache__ -E .vscode -H -t d $args | Invoke-Fzf -Prompt 'cd>' | ForEach-Object { $result = $_ }; if ($null -ne $result) { Set-LocationEx $result } }
  function vff { Invoke-Fzf -Prompt 'gvim>' | % { gvim --remote-silent-tab $_ } }
  function vfr { Get-Content $env:APPDATA\LeaderF\python3\mru\mruCache | Invoke-Fzf -Prompt 'gvim>' | % { gvim --remote-silent-tab $_ } }
}

###############################################################################

If (Get-Command "eza.exe" -ErrorAction SilentlyContinue) {
  function ls { eza }
  function l { eza -al $args }
  function la { eza --all $args }
  function ll { eza --long $args }
  function laht { eza --all --long --sort=modified $args }
} ElseIf (-Not (Test-Path Variable:PSise)) {
  # Only run this in the console and not in the ISE
  Import-Module Get-ChildItemColor

  Set-Alias l Get-ChildItemColor -option AllScope
  Set-Alias ll Get-ChildItemColor -option AllScope
  Set-Alias ls Get-ChildItemColorFormatWide -option AllScope
}

###############################################################################

$env:RIPGREP_CONFIG_PATH = $HOME + "/.ripgreprc"
. $scriptDir/Completions/_rg.ps1

###############################################################################
# ALIASES

remove-item Alias:wget -force -ErrorAction SilentlyContinue
remove-item Alias:curl -force -ErrorAction SilentlyContinue
remove-item Alias:gci -force -ErrorAction SilentlyContinue
remove-item Alias:gp -force -ErrorAction SilentlyContinue
remove-item Alias:gcm -force -ErrorAction SilentlyContinue
remove-item Alias:diff -force -ErrorAction SilentlyContinue


function which($name) {
    Get-Command $name | Select-Object -ExpandProperty Definition
}
Set-Alias realpath Convert-Path


function npm { pnpm $args }

function gst { git status  $args }
function glg { git lg $args }
function gci { git commit $args }
function gcm { git commit -m $args }
function gcam { git commit -a -m $args }
function gcan! { git commit -v -a --no-edit --amend $args }
function gp { git push $args }
function gpa {
  git push --all
  git push --tags
}
function dsf { git diff $args }
function grv { git remote -v $args }
function gfk {
  $fork_url = $(git remote get-url origin | awk -F '/' '{printf "git@github.com:bennyyip/%s", $NF}')
  git remote add fork $fork_url
}
function gget { ghq get --no-recursive --shallow $args }
function gget-full { ghq get $args }
function glook { cd $(Get-ChildItem ~/ghq/github.com/*/* | % { $_.ToString() }  | fzf) }
function gop { bash "~/dotfiles/bin/git-open" $args }

# function vr { gvim --remote-silent-tab ($args | foreach { $_ -replace '\\', '/' }) }
Set-Alias vr "gvim-remote.exe"
# function vr { gvim --remote-silent-tab ($args | foreach  { (Convert-Path $_) -replace '\\', '/' }) }

function rmrf { Remove-Item -Recurse -Force $args }

function rgv { rg --no-ignore-vcs $args }

function update { . $profile }

function Enable-Proxy {
  $env:HTTP_PROXY = $proxy
  $env:HTTPS_PROXY = $proxy
}

function Disable-Proxy {
  $env:HTTP_PROXY = ""
  $env:HTTPS_PROXY = ""
}

function New-Symlink() {
  param (
    [Parameter(Position = 0, Mandatory = $true)]  [String] $src,
    [Parameter(Position = 1, Mandatory = $true)] [String] $dst
  )
  New-Item -type SymbolicLink -Value $(Convert-Path $src) $dst
}

function sshcopyid {
  param(
    [Parameter(Position = 0, Mandatory = $true)] [String] $remote
  )
  Get-Content $env:USERPROFILE\.ssh\id_ed25519.pub | ssh $remote "cat >> .ssh/authorized_keys"
}


function Open-Livestream {
  python $env:USERPROFILE\dotfiles\python\open-livestream.py $args
}
Set-Alias ols Open-Livestream

function rgg {
  python $env:USERPROFILE\bin\rgg $args
}

function agv {
  python $env:USERPROFILE\bin\agv $args
}

function vv {
  python $env:USERPROFILE\dotfiles\bin\vv $args
}


function cdtmp {
    $parent = [System.IO.Path]::GetTempPath()
    $name = 'benyip-' + $([System.IO.Path]::GetRandomFileName()).Split(".")[0]
    New-Item -ItemType Directory -Path (Join-Path $parent $name)
    cd (Join-Path $parent $name)
}

function y {
    $tmp = [System.IO.Path]::GetTempFileName()
    yazi $args --cwd-file="$tmp"
    $cwd = Get-Content -Path $tmp
    if (-not [String]::IsNullOrEmpty($cwd) -and $cwd -ne $PWD.Path) {
        Set-Location -LiteralPath $cwd
    }
    Remove-Item -Path $tmp
}

function Remove-HistoryDuplicates {
  $HASH = @{}
  $newhistory = ""
  Get-Content (Get-PSReadlineOption).HistorySavePath | ` 
    ForEach-Object {
      if ( $HASH.$_ -eq $null ) { $newhistory+=$_ }
      $HASH.$_ = 1 } > $env:TMP\PowerShell-History
  Copy-Item $env:TMP\PowerShell-History (Get-PSReadlineOption).HistorySavePath
}

function Set-Brightness {
    param (
        [Parameter(Position = 0, Mandatory = $false)] [int] $n
    )
    # https://github.com/newAM/monitorcontrol/
    monitorcontrol --set-luminance $n
}


function Syu {
  gsudo winget upgrade --all --verbose
}

function Start-Shizuku {
  adb shell sh /sdcard/Android/data/moe.shizuku.privileged.api/start.sh
}

function m {
  fd -I -t f -S +10M -e mkv -e mp4 -e webm --relative-path . | Invoke-Fzf -Prompt 'mpv>' | % { start-process $_ }
}

function ytdl {
  yt-dlp -N 4 $args
}
