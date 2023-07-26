CapsLock & PgUp:: Send "{Volume_Up}"
CapsLock & Up:: Send "{Volume_Up}"
CapsLock & PgDn:: Send "{Volume_Down}"
CapsLock & Down:: Send "{Volume_Down}"
CapsLock & Left:: Send "{Media_Prev}"
CapsLock & Right:: Send "{Media_Next}"
Pause::Send "{Media_Play_Pause}"
ScrollLock:: {
    active := Launch("C:\Program Files\foobar2000\foobar2000.exe")
    ; if (active) {
    ;     ControlFocus "Edit1"
    ; }
}

PrintScreen:: Launch(A_AppData "\Spotify\Spotify.exe")

#,:: Send "{Media_Play_Pause}"
