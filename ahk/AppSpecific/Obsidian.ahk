global ObsidianPDFMap := false

#HotIf WinActive("ahk_exe obsidian.exe")
XButton2::Send "{right}"
XButton1:: Send "{left}"

^PgUp::
^PgDn:: {
  global ObsidianPDFMap := !ObsidianPDFMap

}

; ^j:: Send "{Down}"
; ^k:: Send "{Up}"
#HotIf

#HotIf WinActive("ahk_exe obsidian.exe") && ObsidianPDFMap
j:: Send "{Down}"
d:: Send "{PgDn}"
k:: Send "{Up}"
u::
+d:: Send "{PgUp}"
#HotIf
