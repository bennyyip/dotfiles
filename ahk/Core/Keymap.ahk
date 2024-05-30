#Include "%A_ScriptDir%\Keymap\Capslock.ahk"
; TODO VDA win 11
; #Include "%A_ScriptDir%\Keymap\VirtualDesktop.ahk"
#Include "%A_ScriptDir%\Keymap\Media.ahk"

; menu
#z:: MyMenu.Show

; Pomodoro
; >^Enter:: Pomodoro
; +>^Enter:: Pomodoro(true)

; others
#+Q:: WinClose WinGetID("A")

#/:: Run "https://duckduckgo.com/?t=ffab&q=" . A_Clipboard

; #c:: Launch "C:\Program Files (x86)\GoldenDict\GoldenDict.exe"
#c:: Run "goldendict://" . A_Clipboard

>#J:: Send "{PgDn}"
>#K:: Send "{PgUp}"

#n:: Send "#+^n"
