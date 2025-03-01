; https://github.com/alacritty/alacritty/issues/2324#issuecomment-608506615
#HotIf WinActive("ahk_exe alacritty.exe")
^+v:: {
    ClipboardBackup := A_Clipboard
    FixString := StrReplace(A_Clipboard, "`r`n", "`n")
    A_Clipboard := FixString
    Send "^+v"
    Sleep 1000
    A_Clipboard := ClipboardBackup
}

; unmap arrow keys
CapsLock & h:: CapsWithMod("h", "^")
CapsLock & j:: CapsWithMod("j", "^")
CapsLock & k:: CapsWithMod("k", "^")
CapsLock & l:: CapsWithMod("l", "^")

XButton1::Send "{H}"
XButton2::Send "{L}"
#HotIf
