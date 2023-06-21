#Requires AutoHotkey v2.0

SendMode "Event"
SetKeyDelay(-1, 30)

#MaxThreadsPerHotkey 3
F1::
{
    static KeepRunning := false
    if KeepRunning  ; This means an underlying thread is already running the loop below.
    {
        KeepRunning := false  ; Signal that thread's loop to stop.
        return  ; End this thread so that the one underneath will resume and see the change made by the line above.
    }
    ; Otherwise:
    KeepRunning := true

    if not WinExist("DARK SOULS III") {
        MsgBox("lauch game first")
        return
    }
    WinActivate

    loop {
        Sleep 2000
        Send "E"
        Sleep 2500
        Send "Q"
        Sleep 2000
        Send "J"
        Sleep 1200
        ; But leave the rest below unchanged.
        if not KeepRunning  ; The user signaled the loop to stop by pressing Win-Z again.
            break  ; Break out of this loop.
    }
    KeepRunning := false  ; Reset in preparation for the next press of this hotkey.
}
#MaxThreadsPerHotkey 1

^F1:: {
    static transparent := false
    if (!transparent) {
        WinSetTransparent 0, "DARK SOULS III"
    } else {
        WinSetTransparent 255, "DARK SOULS III"
    }
    transparent := !transparent
}


^+F1::
{
    ExitApp
}
