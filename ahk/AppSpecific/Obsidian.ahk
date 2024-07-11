#HotIf WinActive("ahk_exe obsidian.exe")
; XButton2::Send "{right}"
; XButton1:: Send "{left}"

XButton1:: {
  saved_clipboard := A_Clipboard
  Send "^c"
  Sleep 100
  Run "goldendict://" . A_Clipboard
  A_Clipboard := saved_clipboard
}
#HotIf

