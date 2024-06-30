#Requires AutoHotkey v2.0
#SingleInstance Force
#Include "utils.ahk"
GAME_TITLE := "ahk_exe eldenring.exe"

farm() {
    SendStrokes 300, "G", "F", "E", "E"
    Sleep 5000
    HoldKey "W", 3500
    HoldKey "A", 800
    HoldKey "W", 1600
    SendThenSleep "Y", 6500
    Sleep 100
}

#MaxThreadsPerHotkey 3
#HotIf WinActive(GAME_TITLE)
F1::
{
    static KeepRunning := false
    if KeepRunning
    {
        KeepRunning := false
        return
    }
    KeepRunning := true

    Sleep 1000

    loop {
        farm()
        if not (KeepRunning and WinActive(GAME_TITLE)) {
            break
        }
    }
    KeepRunning := false
}
#HotIf
#MaxThreadsPerHotkey 1

; zip
F7::
{
    Send "{RButton down}"
    Send "{LAlt Down}"
    AccurateSleep 2195
    Send "{w down}"
    AccurateSleep 116
    Send "{w up}"
    AccurateSleep 300
    Send "{LAlt Up}"
    Send "{RButton up}"
}

