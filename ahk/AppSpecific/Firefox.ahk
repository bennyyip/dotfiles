#HotIf WinActive("ahk_exe firefox.exe")
XButton1:: {
  saved_clipboard := A_Clipboard
  Send "^c"
  Sleep 100
  Run "goldendict://" . A_Clipboard
  A_Clipboard := saved_clipboard
}
^w:: Send "^{Backspace}"
CapsLock & w:: Send "^{Backspace}"
^+w:: Send "^w"
!w:: Send "^w"
#HotIf

