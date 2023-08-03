SetCapsLockState "AlwaysOff"
global OtherKeyPressedDuringCaps := false

CapsLock Up:: Send "{ESC}"

CapsWithMod(action, initmod := "") {
    ; global OtherKeyPressedDuringCaps

    ; OtherKeyPressedDuringCaps := true
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

CapsUp() {
    global OtherKeyPressedDuringCaps
    if (!OtherKeyPressedDuringCaps) {
        Send "{ESC}"
    }
    OtherKeyPressedDuringCaps := false
}

CpasWrap(Fn, args*) {
    ; global OtherKeyPressedDuringCaps

    ; OtherKeyPressedDuringCaps := true
    Fn(args*)
}

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

; Other
CapsLock & LButton:: CpasWrap Send, "^{LButton}"
CapsLock & F11:: CpasWrap Run, "C:\Program Files\AutoHotkey\v2\AutoHotkey.chm"
CapsLock & F12:: CpasWrap Reload

; Media
CapsLock & PgUp::CpasWrap  Send, "{Volume_Up}"
CapsLock & Up:: CpasWrap Send, "{Volume_Up}"
CapsLock & PgDn:: CpasWrap Send, "{Volume_Down}"
CapsLock & Down:: CpasWrap Send, "{Volume_Down}"
CapsLock & Left:: CpasWrap Send, "{Media_Prev}"
CapsLock & Right:: CpasWrap Send, "{Media_Next}"

; Folders
CapsLock & F1:: CpasWrap ShowDir, HOME_DIR . '\Downloads'
CapsLock & F2:: CpasWrap ShowDir, HOME_DIR . '\github'
CapsLock & F3:: CpasWrap ShowDir, HOME_DIR . '\dotfiles'
CapsLock & F4:: CpasWrap ShowDir, HOME_DIR

; App
CapsLock & F5:: CpasWrap Launch, (HOME_DIR . '\AppData\Local\Programs\Anki\anki.exe')
CapsLock & F6:: CpasWrap Launch, ('C:\Program Files (x86)\GoldenDict\GoldenDict.exe')
