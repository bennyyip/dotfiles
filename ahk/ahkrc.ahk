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
#Include "%A_ScriptDir%\keymap.ahk"
; #Include "%A_ScriptDir%\filepilot.ahk"

#Include "local.ahk"

TrayTip "ahkrc started!"
SoundPlay A_WinDir "\Media\speech on.wav"
