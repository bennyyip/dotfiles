$STARTUP_DIR = "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup"

$WshShell = New-Object -comObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut("$STARTUP_DIR\ahkrc.ahk.lnk")
$Shortcut.TargetPath =  $(Convert-Path "./ahk/ahkrc.ahk")
$Shortcut.Save()


New-Item -type SymbolicLink -Value $(Convert-Path .\WindowsPowerShell\) "$env:USERPROFILE/Documents/PowerShell" -Force
New-Item -type SymbolicLink -Value $(Convert-Path .\WindowsPowerShell\) "$env:USERPROFILE/Documents/WindowsPowerShell" -Force
