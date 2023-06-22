; CapsLock::Ctrl
; ~CapsLock Up:: Send("{Ctrl up}" . (A_PriorKey = "CapsLock" ? "{Esc}" : ""))

SetCapsLockState "AlwaysOff"

CapsLock & /:: CapsWithMod("/", "^")
CapsLock & [:: CapsWithMod("[", "^")
CapsLock & ,:: CapsWithMod(",", "^")
CapsLock & ]:: CapsWithMod("]", "^")
CapsLock & a:: CapsWithMod("a", "^")
CapsLock & i:: CapsWithMod("i", "^")
CapsLock & c:: CapsWithMod("c", "^")
CapsLock & d:: CapsWithMod("d", "^")
CapsLock & f:: CapsWithMod("f", "^")
CapsLock & h:: CapsWithMod("Left")
CapsLock & j:: CapsWithMod("Down")
CapsLock & k:: CapsWithMod("Up")
CapsLock & l:: CapsWithMod("Right")
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
CapsLock & `:: CapsWithMod("``", "^")
CapsLock & enter:: CapsWithMod("enter", "^")
CapsLock & Tab:: CapsWithMod("tab", "^")

CapsLock & 1:: Send "#4"
CapsLock & 2:: Send "#5"
CapsLock & 3:: Send "#6"

CapsLock & LButton:: Send "^{LButton}"

CapsLock & F12:: Reload

CapsWithMod(action, initmod := "")
{
    mods := ''
    if GetKeyState("shift")
        mods .= "+"
    Send (initmod . mods . "{" action "}")
    SetCapsLockState "AlwaysOff"
}
