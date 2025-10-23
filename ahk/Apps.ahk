; apps
#HotIf WinActive("ahk_exe alacritty.exe") ; {{{ 1
; https://github.com/alacritty/alacritty/issues/2324#issuecomment-608506615
^+v:: {
    ClipboardBackup := A_Clipboard
    FixString := StrReplace(A_Clipboard, "`r`n", "`n")
    A_Clipboard := FixString
    Send "^+v"
    Sleep 1000
    A_Clipboard := ClipboardBackup
}

; unmap arrow keys
CapsLock & h:: CapsWithMod("h", "^")
CapsLock & j:: CapsWithMod("j", "^")
CapsLock & k:: CapsWithMod("k", "^")
CapsLock & l:: CapsWithMod("l", "^")

XButton1::Send "{H}"
XButton2::Send "{L}"
#HotIf ;}}}
#HotIf WinActive("ahk_exe anki.exe") ; {{{ 1
XButton1::1
XButton2::Space
#HotIf ;}}}
#HotIf WinActive("ahk_exe SumatraPDF.exe") ; {{{ 1
XButton2::d
XButton1:: Send "+d"
#HotIf ;}}}
#HotIf (WinActive("ahk_exe WindowsTerminal.exe") OR WinActive("ahk_exe gvim.exe")) ; {{{ 1
; unmap arrow keys
CapsLock & h:: CapsWithMod("h", "^")
CapsLock & j:: CapsWithMod("j", "^")
CapsLock & k:: CapsWithMod("k", "^")
CapsLock & l:: CapsWithMod("l", "^")
#HotIf ;}}}
#HotIf (WinActive("ahk_exe obsidian.exe") OR WinActive("ahk_exe firefox.exe") OR WinActive("ahk_exe zotero.exe")) ; {{{ 1
XButton1:: {
  saved_clipboard := A_Clipboard
  Send "^c"
  Sleep 100
  Run "goldendict://" . A_Clipboard
  A_Clipboard := saved_clipboard
} #HotIf ;}}}
#HotIf WinActive("ahk_exe StreetFighter6.exe") ; {{{ 1
'::1
#HotIf ;}}}
#HotIf WinActive("ahk_exe bg3_dx11.exe") ; {{{ 1
global __AltState := "UP"
F12:: {
    Send "{Alt Down}"

  global __AltState
  if (__AltState == "UP") {
    Send "{Alt Down}"
    __AltState := "DOWN"
  } else {
    Send "{Alt Up}"
    __AltState := "UP"

  }
}
; ~Alt Up:: {
; }
#HotIf ;}}}
#HotIf WinActive("ahk_exe dota2.exe") ; {{{ 1
; alt -> caps
CapsLock & q:: CapsWithMod("q", "!")
CapsLock & w:: CapsWithMod("w", "!")
CapsLock & e:: CapsWithMod("e", "!")
CapsLock & r:: CapsWithMod("r", "!")
CapsLock & d:: CapsWithMod("d", "!")
CapsLock & f:: CapsWithMod("f", "!")
CapsLock & 1:: CapsWithMod("1", "!")
CapsLock & 2:: CapsWithMod("2", "!")
CapsLock & 3:: CapsWithMod("3", "!")
CapsLock & 4:: CapsWithMod("4", "!")
CapsLock & 5:: CapsWithMod("5", "!")
CapsLock & c:: CapsWithMod("c", "!")
CapsLock & Space:: CapsWithMod("Space", "!")
CapsLock & t:: CapsWithMod("t", "!")
#HotIf ;}}}
#HotIf WinActive("ahk_exe DOOMEternalx64vk.exe") ; {{{ 1
Capslock::4
~CapsLock Up:: Send("{4 up}")
#HotIf ;}}}
#HotIf WinActive("ahk_exe firefox.exe") ; {{{ 1
^w:: Send "^{Backspace}"
CapsLock & w:: Send "^{Backspace}"
^+w:: Send "^w"
!w:: Send "^w"
#HotIf ;}}}
; vim:fdm=marker:fdl=0
