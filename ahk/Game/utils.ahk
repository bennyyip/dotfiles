#Requires AutoHotkey v2.0
; KeyHistory 0
; ListLines false
ProcessSetPriority "A"
SetKeyDelay -1, -1
SetMouseDelay -1
SetDefaultMouseSpeed 0
SetWinDelay -1
SetControlDelay -1
SendMode "Input"

TIME_PER_FRAME_60FPS := 1000 / 60
TIME_PER_FRAME_30FPS := 1000 / 30

AccurateSleep(timeInMs) {
    DllCall("QueryPerformanceFrequency", "Int64*", &freq := 0)
    DllCall("QueryPerformanceCounter", "Int64*", &CounterBefore := 0)
    DllCall("QueryPerformanceCounter", "Int64*", &CounterAfter := 0)

    if (timeInMs > 1000) {
        Sleep timeInMs - 500
    }

    while (((CounterAfter - CounterBefore) / freq * 1000) < timeInMs)
    {
        DllCall("QueryPerformanceCounter", "Int64*", &CounterAfter)
    }
}

KeyDown(key) {
    Send Format("{{}{} down{}}", key)
}

KeyUp(key) {
    Send Format("{{}{} up{}}", key)
}

HoldKey(key, holdTimeInMs, sleepTimeInMs := 0) {
    KeyDown key
    Sleep holdTimeInMs
    KeyUp key
    if sleepTimeInMs > 0 {
        sleep sleepTimeInMs
    }
}

PressKey(key) {
    HoldKey key, 2 * TIME_PER_FRAME_60FPS
}

SendStrokes(delayInMs, keys*) {
    for index, key in keys {
        PressKey key
        Sleep delayInMs
    }
}

SendThenSleep(key, timeInMs) {
    PressKey key
    Sleep timeInMs
}
