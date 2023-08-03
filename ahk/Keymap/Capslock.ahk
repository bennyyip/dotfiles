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

; Ctrl
CapsLock & ,:: CapsWithMod(",", "^")
CapsLock & -:: CapsWithMod("=", "^")
CapsLock & /:: CapsWithMod("/", "^")
CapsLock & =:: CapsWithMod("-", "^")
CapsLock & BackSpace:: CapsWithMod("backspace", "^")
CapsLock & Tab:: CapsWithMod("tab", "^")
CapsLock & [:: CapsWithMod("[", "^")
CapsLock & ]:: CapsWithMod("]", "^")
CapsLock & `:: CapsWithMod("``", "^")
CapsLock & enter:: CapsWithMod("enter", "^")

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
CapsLock & F11:: Run "C:\Program Files\AutoHotkey\v2\AutoHotkey.chm"
CapsLock & F12:: Reload

; Media
CapsLock & PgUp:: Send "{Volume_Up}"
CapsLock & Up:: Send "{Volume_Up}"
CapsLock & PgDn:: Send "{Volume_Down}"
CapsLock & Down:: Send "{Volume_Down}"
CapsLock & Left:: Send "{Media_Prev}"
CapsLock & Right:: Send "{Media_Next}"

; Folders
CapsLock & F1:: ShowDir HOME_DIR . '\Downloads'
CapsLock & F2:: ShowDir HOME_DIR . '\github'
CapsLock & F3:: ShowDir HOME_DIR . '\dotfiles'
CapsLock & F4:: ShowDir HOME_DIR

; App
CapsLock & F5:: Launch (HOME_DIR . '\AppData\Local\Programs\Anki\anki.exe')
CapsLock & F6:: Launch ('C:\Program Files (x86)\GoldenDict\GoldenDict.exe')
