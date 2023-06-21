#Requires AutoHotkey v2.0

SendMode "Event"
SetKeyDelay(-1, 30)

SendStrokes(delayInMs, keys*) {
    for index, key in keys {
        Send key
        Sleep delayInMs
    }
}

IsLoading() {
    if PixelSearch(&Px, &Py, 0, 0, 270, 140, 0x1c1c40, 10) {
        return false
    } else {
        return true
    }
}

WaitLoading() {
    loop {
        Sleep(100)
    } until !IsLoading()
    Sleep 500
}

F4:: {
    WaitLoading
    SendStrokes 100, "{ESC}", "{left}", "E", "{shift}", "+{left}", "E", "{left}"
    Sleep 200
    Send "E"
    SendStrokes 500, "!{tab}", "{space}", "!{tab}"
    Sleep 10000
    SendStrokes 2000, "E", "E", "E", "E"
}

F5:: {
    WaitLoading
    Send "{ctrl}"
    Sleep 3000
    SendStrokes 200, "{right}", "J"
    Sleep 3000
    SendStrokes 800, "{right}", "F"
}
