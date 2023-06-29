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

; Pomodoro
>#Enter:: Pomodoro
+>#Enter:: Pomodoro(true)

; others
#+E:: _CloseDuplicateExplorerWindows()

#+Q:: WinClose WinGetID("A")

#/:: Run "https://duckduckgo.com/?t=ffab&q=" . A_Clipboard

#h:: _PasteToVim()

#c:: Launch "C:\Program Files (x86)\GoldenDict\GoldenDict.exe"

_PasteToVim() {
    draftFile := HOME_DIR . "/temp/" . FormatTime(, "yyyy-MM-dd") . ".txt"
    RunWait "gvim.exe --remote " . draftFile
    SendText 'G] j"+p'
}

_CloseDuplicateExplorerWindows() {
    ws := WinGetList("ahk_class CabinetWClass")
    winSet := Map()
    for w in ws {
        title := WinGetTitle(w)
        if winSet.Has(title) {
            WinClose w
        } else {
            winSet[title] := 1
        }
    }
}
