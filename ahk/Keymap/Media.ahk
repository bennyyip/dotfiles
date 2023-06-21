CapsLock & PgUp:: Send "{Volume_Up}"
CapsLock & Up:: Send "{Volume_Up}"
CapsLock & PgDn:: Send "{Volume_Down}"
CapsLock & Down:: Send "{Volume_Down}"
CapsLock & Left:: Send "{Media_Prev}"
CapsLock & Right:: Send "{Media_Next}"
Pause::Send "{Media_Play_Pause}"
PrintScreen:: Launch("ahk_exe Spotify.exe", A_AppData "\Spotify\Spotify.exe")
ScrollLock:: Launch("ahk_exe foobar2000.exe", "C:\Program Files\foobar2000\foobar2000.exe")

#,:: Send "{Media_Play_Pause}"

Launch(title, exe) {
    if WinActive(title) {
        send "!{tab}"
        WinMinimize title
    } else {
        if WinExist(title) {
            WinActivate
        }
        else {
            Run exe
        }
    }
}
