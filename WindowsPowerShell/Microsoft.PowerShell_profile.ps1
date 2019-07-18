$scriptDir = Split-Path -Path $MyInvocation.MyCommand.Definition -Parent

$env:_ZL_NO_PROMPT_COMMAND=1
iex ($(lua53 $scriptDir\Contrib\z.lua --init powershell) -join "`n")

###############################################################################

import-module $scriptDir\prompt.psm1

function prompt {
	gitFancyPrompt
        _zlua --update
}

###############################################################################

Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs

Set-PSReadLineOption -HistoryNoDuplicates
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineOption -HistorySaveStyle SaveIncrementally
Set-PSReadLineOption -MaximumHistoryCount 4000
# history substring search
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# Tab completion
Set-PSReadlineKeyHandler -Chord 'Shift+Tab' -Function Complete
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

Set-PSReadlineKeyHandler -Key Ctrl+Backspace -Function UnixWordRubout

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

Import-Module Get-ChildItemColor

###############################################################################

. $scriptDir/Completions/_rg.ps1

###############################################################################
# ALIASES

Set-Alias l Get-ChildItemColor -option AllScope
Set-Alias ls Get-ChildItemColorFormatWide -option AllScope

remove-item Alias:gci -force -ErrorAction SilentlyContinue
remove-item Alias:gp -force -ErrorAction SilentlyContinue
function gst { git status  $args }
function glg { git lg $args }
function gci { git commit $args }
function gcam { git commit -a -m $args }
function gcan! { git commit -v -a --no-edit --amend $args }
function gp { git push $args }
function dsf { git diff $args }
function grv { git remote -v $args }
