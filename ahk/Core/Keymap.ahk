#Include "%A_ScriptDir%\Keymap\Capslock.ahk"
#Include "%A_ScriptDir%\Keymap\VirtualDesktop.ahk"
#Include "%A_ScriptDir%\Keymap\Media.ahk"

; menu
#z:: MyMenu.Show

; folders
CapsLock & F1:: ShowDir(HOME_DIR . '\Downloads')
CapsLock & F2:: ShowDir(HOME_DIR . '\github')
CapsLock & F3:: ShowDir(HOME_DIR . '\dotfiles')
CapsLock & F4:: ShowDir(HOME_DIR)

CapsLock & F5:: Launch(HOME_DIR . '\AppData\Local\Programs\Anki\anki.exe')

; Pomodoro
>#Enter:: Pomodoro
+>#Enter:: Pomodoro(true)

; others
#+Q:: WinClose WinGetID("A")

#/:: Run "https://duckduckgo.com/?t=ffab&q=" . A_Clipboard

#c:: Launch "C:\Program Files (x86)\GoldenDict\GoldenDict.exe"
