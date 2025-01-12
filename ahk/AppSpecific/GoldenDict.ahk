#HotIf (WinActive("ahk_exe obsidian.exe")
OR WinActive("ahk_exe firefox.exe")
OR WinActive("ahk_exe zotero.exe")
)

XButton1:: {
  saved_clipboard := A_Clipboard
  Send "^c"
  Sleep 100
  Run "goldendict://" . A_Clipboard
  A_Clipboard := saved_clipboard
}
#HotIf
