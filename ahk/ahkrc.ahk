#Requires AutoHotkey v2.0
#SingleInstance Force
SetWorkingDir(A_ScriptDir)
HOME_DIR := "C:\Users\" . A_UserName
VIM := "gvim --remote "
EDITOR := VIM

A_HotkeyInterval := 999999999  ; This is the default value (2000 milliseconds).
A_MaxHotkeysPerInterval := 99999999999 ; default 200

#Include "Core/Functions.ahk"
#Include "Core/AppSpecific.ahk"
#Include "Core/MyMenu.ahk"
#Include "Core/Keymap.ahk"

TrayTip "ahkrc started!"


; #Include "komorebic\komorebi.ahk"
