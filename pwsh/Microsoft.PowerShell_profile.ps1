$scriptDir = Split-Path -PaTh $MyInvocation.MyCommand.Definition -Parent
$env:PAGER = 'less.exe'

$env:EDITOR = "vim"
$env:PATH = $env:PATH + ";C:\Program Files\starship\bin;$HOME/bin"

###############################################################################
# https://www.lua.org/download.html

if (Get-Command "starship.exe" -ErrorAction SilentlyContinue) {
    $env:STARSHIP_CONFIG = "$env:USERPROFILE\.config\starship.toml"
    Invoke-Expression (&starship init powershell)
    Invoke-Expression (& { (lua53 $scriptDir\Contrib\z.lua --init powershell) -join "`n" })
}
else {
    import-module $scriptDir\prompt.psm1

    Invoke-Expression ($(lua53 $scriptDir\Contrib\z.lua --init powershell) -join "`n")

    function prompt {
        gitFancyPrompt
        _zlua --update
    }
}

###############################################################################

Import-Module PSReadLine
Set-PSReadLineOption -EditMode vi

Set-PSReadLineOption -HistoryNoDuplicates
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineOption -HistorySaveStyle SaveIncrementally
Set-PSReadLineOption -MaximumHistoryCount 4000
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
Set-PSReadLineKeyHandler -Key   Ctrl+w          -Function UnixWordRubout
Set-PSReadlineKeyHandler -Chord 'Ctrl+x,Ctrl+e' -Function ViEditVisually
Set-PSReadlineKeyHandler -Key   Ctrl+Backspace  -Function UnixWordRubout

Set-PSReadlineKeyHandler -Chord Ctrl+V -ScriptBlock {
    $clipboard = Get-Clipboard -Raw
    if ($clipboard -match '^\s*(http|ftp|magnet)' -or `
        (Test-Path $clipboard.Trim()) ) {
        $clipboard = $clipboard.Trim()
        $clipboard = "'${clipboard}'"
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
    function scd { $result = $null; fd -E .git -E __pycache__ -E .vscode -H -t d | Invoke-Fzf -Prompt 'cd>' | ForEach-Object { $result = $_ }; if ($null -ne $result) { Set-LocationEx $result } }
    function vff { Invoke-Fzf -Prompt 'gvim>' | % { gvim --remote $_ } }
    function vfr { Get-Content $HOME/.LfCache/python3/mru/mruCache | Invoke-Fzf -Prompt 'gvim>' | % { gvim --remote $_ } }
}

###############################################################################

If (-Not (Test-Path Variable:PSise)) {
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

Set-Alias which Get-Command
Set-Alias realpath Convert-Path


function npm { pnpm $args }

function gst { git status  $args }
function glg { git lg $args }
function gci { git commit $args }
function gcm { git commit -m $args }
function gcam { git commit -a -m $args }
function gcan! { git commit -v -a --no-edit --amend $args }
function gp { git push $args }
function dsf { git diff $args }
function grv { git remote -v $args }
function gfk {
    $fork_url = $(git remote get-url origin | awk -F '/' '{printf "git@github.com:bennyyip/%s", $NF}')
    git remote add fork $fork_url
}

function vr { gvim --remote $args }

function zz { z -i $args }
function zc { z -c $args }
function zf { z -I $args }
function zb { z -b $args }
function zbi { z -b -i $args }
function zbf { z -b -I $args }
function zh { z -I -t . $args }
function zzc { zz -c $args }

function rmrf { Remove-Item -Recurse -Force $args }

function rgv { rg --no-ignore-vcs $args }

function update { . $profile }

$proxy = "http://127.0.0.1:10809"
function Enable-Proxy {
    $env:HTTP_PROXY = $proxy
    $env:HTTPS_PROXY = $proxy
}

function Disable-Proxy {
    $env:HTTP_PROXY = ""
    $env:HTTPS_PROXY = ""
}

function vimv {
    $tempfile = New-TemporaryFile

    if ($args.Length -gt 0) {
        $src = $args
    }
    else {
        $src = Get-ChildItem -name
    }
    Write-Output $src > $tempfile

    vim $tempfile
    if ($LASTEXITCODE -ne 0) {
        Write-Output "WARN: Vim exit code $LASTEXITCODE. Aborting.."
        return
    }

    $dst = Get-Content $tempfile
    if ($src.Length -ne $dst.Length) {
        Write-Output "WARN: Number of files changed. Did you delete a line by accident? Aborting.."
        return
    }

    $count = 0
    for ($i = 0; $i -lt $src.Length; $i++) {
        if ($src[$i] -eq $dst[$i]) {
            continue
        }
        $count ++
        $p = Split-Path -Parent $dst[$i]
        if ($p -ne "" ) {
            Mkdir -Force $p | Out-Null
        }
        Move-Item $src[$i] $dst[$i]
    }

    Remove-Item $tempfile

    Write-Output "$count files renamed."
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

