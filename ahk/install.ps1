$STARTUP_DIR = "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup"

$WshShell = New-Object -comObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut("$STARTUP_DIR\ahkrc.ahk.lnk")
$Shortcut.TargetPath =  $(Convert-Path $PSScriptRoot\"ahkrc.ahk")
$Shortcut.Save()

