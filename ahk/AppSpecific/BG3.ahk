#HotIf WinActive("ahk_exe bg3_dx11.exe")
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
#HotIf
