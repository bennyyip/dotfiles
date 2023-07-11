; CapsLock::Ctrl
; ~CapsLock Up:: Send("{Ctrl up}" . (A_PriorKey = "CapsLock" ? "{Esc}" : ""))

SetCapsLockState "AlwaysOff"

; Ctrl
CapsLock & ,:: CapsWithMod(",", "^")
CapsLock & /:: CapsWithMod("/", "^")
CapsLock & BackSpace:: CapsWithMod("backspace", "^")
CapsLock & Tab:: CapsWithMod("tab", "^")
CapsLock & [:: CapsWithMod("[", "^")
CapsLock & ]:: CapsWithMod("]", "^")
CapsLock & `:: CapsWithMod("``", "^")
CapsLock & a:: CapsWithMod("a", "^")
CapsLock & c:: CapsWithMod("c", "^")
CapsLock & d:: CapsWithMod("d", "^")
CapsLock & e:: CapsWithMod("e", "^")
CapsLock & enter:: CapsWithMod("enter", "^")
CapsLock & f:: CapsWithMod("f", "^")
CapsLock & i:: CapsWithMod("i", "^")
CapsLock & n:: CapsWithMod("n", "^")
CapsLock & o:: CapsWithMod("o", "^")
CapsLock & p:: CapsWithMod("p", "^")
CapsLock & q:: CapsWithMod("q", "^")
CapsLock & r:: CapsWithMod("r", "^")
CapsLock & t:: CapsWithMod("t", "^")
CapsLock & u:: CapsWithMod("u", "^")
CapsLock & v:: CapsWithMod("v", "^")
CapsLock & w:: CapsWithMod("w", "^")
CapsLock & x:: CapsWithMod("x", "^")

; Win
CapsLock & 1:: CapsWithMod("4", "#")
CapsLock & 2:: CapsWithMod("5", "#")
CapsLock & 3:: CapsWithMod("6", "#")

; hjkl
CapsLock & h:: CapsWithMod("Left")
CapsLock & j:: CapsWithMod("Down")
CapsLock & k:: CapsWithMod("Up")
CapsLock & l:: CapsWithMod("Right")

; other
CapsLock & LButton:: Send "^{LButton}"
CapsLock & F11:: Run "C:\Program Files\AutoHotkey\v2\AutoHotkey.chm"
CapsLock & F12:: Reload

CapsWithMod(action, initmod := "")
{
    mods := ''
    if GetKeyState("shift")
        mods .= "+"
    if GetKeyState("ctrl")
        mods .= "^"
    Send (initmod . mods . "{" action "}")
    SetCapsLockState "AlwaysOff"
}
