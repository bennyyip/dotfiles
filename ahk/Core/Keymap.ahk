#Include "%A_ScriptDir%\Keymap\Capslock.ahk"
#Include "%A_ScriptDir%\Keymap\VirtualDesktop.ahk"
#Include "%A_ScriptDir%\Keymap\Media.ahk"

; menu
#z::MyMenu.Show

; folders
CapsLock & F1:: ShowDir(HOME_DIR . '\Downloads')
CapsLock & F2:: ShowDir(HOME_DIR . '\github')
CapsLock & F3:: ShowDir(HOME_DIR . '\dotfiles')
CapsLock & F4:: ShowDir(HOME_DIR)

; Pomodoro
>#Enter:: Pomodoro
+>#Enter:: Pomodoro(true)

; others
#+E:: CloseDuplicateExplorerWindows()

#+Q:: WinClose WinGetID("A")

#/::Run "https://duckduckgo.com/?t=ffab&q=" . A_Clipboard

#h::PasteToVim()
