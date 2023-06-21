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
F1::
{
    static KeepRunning := false
    if KeepRunning
    {
        KeepRunning := false
        return
    }
    KeepRunning := true

    if not WinExist(GAME_TITLE) {
        MsgBox("lauch game first")
        return
    }
    WinActivate
    Sleep 1000

    loop {
        farm()
        if not (KeepRunning and WinActive(GAME_TITLE)) {
            break
        }
    }
    KeepRunning := false
}
#MaxThreadsPerHotkey 1

^+F1::
{
    ExitApp
}

F2::
{
    if not WinExist(GAME_TITLE) {
        MsgBox("lauch game first")
        return
    }
    WinKill GAME_TITLE
    sleep 1000
    ProcessClose "eldenring.exe"
    sleep 1000
    Run "C:\Program Files\PowerShell\7\pwsh.exe -NoLogo -NoProfile -File C:\Users\bennyyip\Desktop\er.ps1"
}

F3:: {
    Run "C:\Program Files\PowerShell\7\pwsh.exe -NoLogo -NoProfile -File C:\Users\bennyyip\Desktop\er.ps1"

}
