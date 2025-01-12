#HotIf WinActive("ahk_exe firefox.exe")
^w:: Send "^{Backspace}"
CapsLock & w:: Send "^{Backspace}"
^+w:: Send "^w"
!w:: Send "^w"

#HotIf

