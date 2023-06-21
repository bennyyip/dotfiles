CapsLock & PgUp:: Send "{Volume_Up}"
CapsLock & Up:: Send "{Volume_Up}"
CapsLock & PgDn:: Send "{Volume_Down}"
CapsLock & Down:: Send "{Volume_Down}"
CapsLock & Left:: Send "{Media_Prev}"
CapsLock & Right:: Send "{Media_Next}"
Pause::Send "{Media_Play_Pause}"
ScrollLock:: Launch("C:\Program Files\foobar2000\foobar2000.exe")
PrintScreen:: Launch(A_AppData "\Spotify\Spotify.exe")

#,:: Send "{Media_Play_Pause}"

Launch(fullpath) {
    SplitPath fullpath, &exe_name
    title := "ahk_exe " . exe_name
    if WinActive(title) {
        WinMinimize title
    } else {
        if WinExist(title) {
            WinActivate
        }
        else {
            Run fullpath
        }
    }
}
