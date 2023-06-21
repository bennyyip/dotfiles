#Include "%A_ScriptDir%\Keymap\Capslock.ahk"
#Include "%A_ScriptDir%\Keymap\VirtualDesktop.ahk"
#Include "%A_ScriptDir%\Keymap\Media.ahk"

; folders
CapsLock & F1:: ShowDir(HOME_DIR . '\Downloads')
CapsLock & F2:: ShowDir(HOME_DIR . '\github')
CapsLock & F3:: ShowDir(HOME_DIR . '\dotfiles')
CapsLock & F4:: ShowDir(HOME_DIR)

; others

#+E:: CloseDuplicateExplorerWindows()

#F:: Run "C:\Program Files\Everything\Everything.exe", , "-filename " A_Clipboard

#+Q:: WinClose WinGetID("A")

#/::Run "https://duckduckgo.com/?t=ffab&q=" . A_Clipboard

#h::PasteToVim()
