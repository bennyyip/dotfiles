#Requires AutoHotkey v2.0
#SingleInstance Force
SetWorkingDir(A_ScriptDir)
HOME_DIR := "C:\Users\" . A_UserName
VIM := "gvim --remote-silent-tab "
EDITOR := VIM

A_HotkeyInterval := 999999999  ; This is the default value (2000 milliseconds).
A_MaxHotkeysPerInterval := 99999999999 ; default 200

; INCLUDE Common FIRST
#Include "%A_ScriptDir%\lib\Common.ahk"
#Include "%A_ScriptDir%\lib\VDA.ahk"

; Apps
#Include "%A_ScriptDir%\Apps.ahk"
#Include "%A_ScriptDir%\Everything.ahk"

; Keymap
#Include "%A_ScriptDir%\MyMenu.ahk"
#Include "%A_ScriptDir%\Keymap\Capslock.ahk"
#Include "%A_ScriptDir%\Keymap\VirtualDesktop.ahk"
#Include "%A_ScriptDir%\Keymap\Media.ahk"
#Include "%A_ScriptDir%\Keymap\Vim.ahk"

; menu
#z:: MyMenu.Show

; others
#+Q:: WinClose WinGetID("A")

#/:: Run "https://duckduckgo.com/?t=ffab&q=" . A_Clipboard

; #c:: Launch "C:\Program Files (x86)\GoldenDict\GoldenDict.exe"
#c:: Run "goldendict://" . A_Clipboard

>#J:: Send "{PgDn}"
>#K:: Send "{PgUp}"

#n:: Send "#+^n"

#e:: ShowDir ""

#Include "local.ahk"

TrayTip "ahkrc started!"
