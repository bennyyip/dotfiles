; keymap
; MyMenu {{{1
MyMenu := Menu()

MyMenu.Add "Paste into &HTML and Open", MyMenuItems.ClipToBrowser
MyMenu.Add "Translate with &DeepL", MyMenuItems.DeepL
MyMenu.Add "Translate with &Google Translate", MyMenuItems.GoogleTranslate
MyMenu.Add "Open win MP&V", MyMenuItems.MPV
MyMenu.Add "&Paste to Vim", MyMenuItems.PasteToVim
MyMenu.Add "&Fix keyboard layout", MyMenuItems.FixKeyboardLayout

killMenu := Menu()
killMenu.Add "&All explorers", MyMenuItems.KillAllExplorers
killMenu.Add "&Duplicate explorers", MyMenuItems.KillDuplicateExplorers
MyMenu.Add "&Kill", killMenu


class MyMenuItems {
    static ClipToBrowser(*) {
        f := FileOpen(A_Temp "\temp.html", "w")
        f.Write("<pre>" A_Clipboard "</pre>")
        f.Close()
        Run (A_Temp "\temp.html")
    }

    static DeepL(*) {
        Run "https://www.deepl.com/translator#en/zh/" . A_Clipboard
    }

    static GoogleTranslate(*) {
        Run "https://translate.google.com/?sl=auto&tl=zh-CN&op=translate&text=" . A_Clipboard
    }

    static MPV(*) {
        Run "mpv " . A_Clipboard
    }

    static KillAllExplorers(*) {
        wins := WinGetList("ahk_class CabinetWClass")
        subMenu := Menu()
        for w in wins {
            try {
                WinClose w
            }
        }

        wins := WinGetList("ahk_class File Pilot")
        subMenu := Menu()
        for w in wins {
            try {
                WinClose w
            }
        }
    }

    static KillDuplicateExplorers(*) {
        ws := WinGetList("ahk_class CabinetWClass")
        winSet := Map()
        for w in ws {
            title := WinGetTitle(w)
            if winSet.Has(title) {
                try {
                    WinClose w
                }
            } else {
                winSet[title] := 1
            }
        }
    }

    static PasteToVim(*) {
        Run "gvim --remote-send :Capture<CR>"
    }

    static FixKeyboardLayout(*) {
        Run "pwsh -noProfile -NoLogo " .  "C:\Users\" . A_UserName . "/dotfiles/pwsh/fix-keyboard-layout.ps1", "", "Hide"
    }

}
; }}}
; Capslock {{{1
SetCapsLockState "AlwaysOff"

CapsLock Up:: Send "{ESC}"

CapsWithMod(action, initmod := "") {
    mods := ''
    if GetKeyState("shift")
        mods .= "+"
    if GetKeyState("ctrl")
        mods .= "^"
    if GetKeyState("alt")
        mods .= "!"
    Send (initmod . mods . "{" action "}")
    SetCapsLockState "AlwaysOff"
}


HideShowTaskbar() {
    static ABM_SETSTATE := 0xA, ABS_AUTOHIDE := 0x1, ABS_ALWAYSONTOP := 0x2
    static hide := 0
    hide := !hide
    APPBARDATA := Buffer(size := 2*A_PtrSize + 2*4 + 16 + A_PtrSize, 0)
    NumPut("UInt", size, APPBARDATA), NumPut("Ptr", WinExist("ahk_class Shell_TrayWnd"), APPBARDATA, A_PtrSize)
    NumPut("UInt", hide ? ABS_AUTOHIDE : ABS_ALWAYSONTOP, APPBARDATA, size - A_PtrSize)
    DllCall("Shell32\SHAppBarMessage", "UInt", ABM_SETSTATE, "Ptr", APPBARDATA)
}


; Ctrl
CapsLock & ,:: CapsWithMod(",", "^")
CapsLock & -:: CapsWithMod("-", "^")
CapsLock & /:: CapsWithMod("/", "^")
CapsLock & =:: CapsWithMod("=", "^")
CapsLock & BackSpace:: CapsWithMod("backspace", "^")
CapsLock & Tab:: CapsWithMod("tab", "^")
CapsLock & [:: CapsWithMod("[", "^")
CapsLock & ]:: CapsWithMod("]", "^")
CapsLock & `:: CapsWithMod("``", "^")
CapsLock & enter:: CapsWithMod("enter", "^")
CapsLock & \:: CapsWithMod("\", "^")

CapsLock & a:: CapsWithMod("a", "^")
CapsLock & b:: CapsWithMod("b", "^")
CapsLock & c:: CapsWithMod("c", "^")
CapsLock & d:: CapsWithMod("d", "^")
CapsLock & e:: CapsWithMod("e", "^")
CapsLock & f:: CapsWithMod("f", "^")
CapsLock & g:: CapsWithMod("g", "^")
CapsLock & i:: CapsWithMod("i", "^")
CapsLock & m:: CapsWithMod("m", "^")
CapsLock & n:: CapsWithMod("n", "^")
CapsLock & o:: CapsWithMod("o", "^")
CapsLock & p:: CapsWithMod("p", "^")
CapsLock & q:: CapsWithMod("q", "^")
CapsLock & r:: CapsWithMod("r", "^")
CapsLock & s:: CapsWithMod("s", "^")
CapsLock & t:: CapsWithMod("t", "^")
CapsLock & u:: CapsWithMod("u", "^")
CapsLock & v:: CapsWithMod("v", "^")
CapsLock & w:: CapsWithMod("w", "^")
CapsLock & x:: CapsWithMod("x", "^")
CapsLock & y:: CapsWithMod("y", "^")
CapsLock & z:: CapsWithMod("z", "^")

; hjkl
CapsLock & h:: CapsWithMod("Left")
CapsLock & j:: CapsWithMod("Down")
CapsLock & k:: CapsWithMod("Up")
CapsLock & l:: CapsWithMod("Right")

; Win
CapsLock & 1:: CapsWithMod("4", "#")
CapsLock & 2:: CapsWithMod("5", "#")
CapsLock & 3:: CapsWithMod("6", "#")

; Other
CapsLock & LButton:: Send "^{LButton}"
CapsLock & F10::HideShowTaskbar()
CapsLock & F11:: Run "C:\Program Files\AutoHotkey\v2\AutoHotkey.chm"
CapsLock & F12:: {
    if (A_IsCompiled) {
        msgbox "compiled ahkrc cannot be reloaded"
    } else {
        Reload
    }
}

; Media
CapsLock & PgUp:: Send "{Volume_Up}"
CapsLock & Up:: Send "{Volume_Up}"
CapsLock & PgDn:: Send "{Volume_Down}"
CapsLock & Down:: Send "{Volume_Down}"
CapsLock & Left:: Send "{Media_Prev}"
CapsLock & Right:: Send "{Media_Next}"

; Folders
CapsLock & F1:: ShowDir HOME_DIR . '\Downloads'
CapsLock & F2:: ShowDir HOME_DIR . '\dotfiles'
CapsLock & F3:: ShowDir HOME_DIR

; App
CapsLock & F5:: Launch (HOME_DIR . '\AppData\Local\Programs\Anki\anki.exe')
CapsLock & F6:: Launch ('C:\Program Files (x86)\GoldenDict\GoldenDict.exe')


; }}}
; Media {{{1
Pause::Send "{Media_Play_Pause}"
ScrollLock:: Launch("C:\Program Files\foobar2000\foobar2000.exe")


CapsLock & PrintScreen::
^PrintScreen:: Launch(A_AppData "\Spotify\Spotify.exe")
; disable printscrren in Accessibility > keyboard
PrintScreen:: Launch(A_AppData "\Spotify\Spotify.exe")

#,:: Send "{Media_Play_Pause}"
; }}}
; Vim {{{1
global VimNormalMapping := false

CapsLock & Esc:: {
  global VimNormalMapping := !VimNormalMapping
  if (VimNormalMapping) {
    TraySetIcon "gvim.exe"
  } else {
    TraySetIcon A_AhkPath
  }
}

#HotIf VimNormalMapping
h:: Send "{Left}"
j:: Send "{Down}"
k:: Send "{Up}"
l:: Send "{Right}"
d:: Send "{PgDn}"
u::
+d:: Send "{PgUp}"
g:: Send "{Home}"
+g:: Send "{End}"
#HotIf
; }}}
; VDA {{{1
#F1:: MoveOrGotoDesktopNumber(0)
#F2:: MoveOrGotoDesktopNumber(1)
#F3:: MoveOrGotoDesktopNumber(2)
#F4:: MoveOrGotoDesktopNumber(3)
#F5:: MoveOrGotoDesktopNumber(4)
#+F1:: MoveCurrentWindowToDesktop(0)
#+F2:: MoveCurrentWindowToDesktop(1)
#+F3:: MoveCurrentWindowToDesktop(2)
#+F4:: MoveCurrentWindowToDesktop(3)
#+F5:: MoveCurrentWindowToDesktop(4)
#+tab:: MoveOrGoToLastOpenedDesktop()

#Q:: GoToPrevDesktop()
#W:: GoToNextDesktop()
; }}}
; Everything {{{1
class Everything {
    static exe := 'C:\Program Files\Everything\Everything.exe'

    static searchExecutabe() {
        Run this.exe . ' -filter executable -sort "run count" -sort-descending'
    }
    static searchEverything() {
        Run this.exe . ' -filter everything -sort "Date Modified" -sort-descending'
    }
    static searchFolder() {
        Run this.exe ' -filter Folder -sort "Date Modified" -sort-descending'
    }

    ; Open file in vim, dir in terminal
    static OpenInVimOrTerminal() {
        row := ListViewGetContent("Focused", "SysListView321", "ahk_class EVERYTHING_(1.5a)")
        parts := StrSplit(row, "`t")
        fullpath := Quote(parts[2] . '\' . parts[1])
        entryType := parts[4]
        if (entryType = "File Folder") {
            Run "wt new-tab -d " . fullpath
        } else {
            Run EDITOR . fullpath
        }
    }

    ; Copy parent path
    static CopyParentPath() {
        row := ListViewGetContent("Focused", "SysListView321")
        parts := StrSplit(row, "`t")
        parentPath := parts[2]
        A_Clipboard := parentPath
    }

    ; Run terminal in parent dir
    static OpenParentInTerminal() {
        row := ListViewGetContent("Focused", "SysListView321")
        parts := StrSplit(row, "`t")
        Run "wt new-tab -d " . Quote(parts[2])
    }
}

#F:: Everything.searchEverything
#+F:: Everything.searchExecutabe

#HotIf WinActive("ahk_class EVERYTHING")

+Enter:: Everything.OpenInVimOrTerminal
+^Enter:: Everything.OpenParentInTerminal

CapsLock & C:: Everything.CopyParentPath

; Run
!R:: Everything.searchExecutabe
; All
!A:: Everything.searchEverything
; Dir
!D:: Everything.searchFolder

#HotIf

; }}}

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

; win+shift+t toggle always on top
#+T:: {
    WinSetAlwaysOnTop -1, "A"
    if (WinGetAlwaysOnTop("A")) {
        SoundPlay A_WinDir "\Media\speech on.wav"
    } else {
        SoundPlay A_WinDir "\Media\speech off.wav"
    }
}
; #e:: ShowDir ""

#E:: {
If WinExist("ahk_exe FPilot.exe")
    WinActivate
else
    Run "C:\Users\bennyyip\AppData\Local\Voidstar\FilePilot\FPilot.exe"
return
}

; vim:fdm=marker
