#Requires AutoHotkey v2.0
#SingleInstance Force
SetWorkingDir(A_ScriptDir)
HOME_DIR := "C:\Users\" . A_UserName

A_HotkeyInterval := 999999999  ; This is the default value (2000 milliseconds).
A_MaxHotkeysPerInterval := 99999999999 ; default 200

#Include "VDA.ahk"

CapsLock::Ctrl
~CapsLock Up:: Send("{Ctrl up}" . (A_PriorKey = "CapsLock" ? "{Esc}" : ""))

#HotIf WinActive("ahk_exe dota2.exe")
Capslock::Alt
~CapsLock Up:: Send("{Alt up}")
#HotIf


#HotIf WinActive("ahk_exe DOOMEternalx64vk.exe")
Capslock::4
~CapsLock Up:: Send("{4 up}")
#HotIf

#HotIf WinActive("ahk_exe SumatraPDF.exe")
XButton2::d
XButton1::Send "+d"
#HotIf

#HotIf WinActive("ahk_exe anki.exe")
XButton1::1
XButton2::Space
#HotIf


#F:: Run "C:\Program Files\Everything\Everything.exe"

#S:: Send "#4"

#,:: Send "{Media_Play_Pause}"
#+Q:: WinClose WinGetID("A")

#+E:: CloseDuplicateExplorerWindows()

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
#Q:: GoToPrevDesktop()
#W:: GoToNextDesktop()
#+tab:: MoveOrGoToLastOpenedDesktop()

CloseDuplicateExplorerWindows() {
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
