global ObsidianPDFMap := false

#HotIf WinActive("ahk_exe obsidian.exe")
^PgUp::
^PgDn:: {
  global ObsidianPDFMap := !ObsidianPDFMap
}
#HotIf

#HotIf WinActive("ahk_exe obsidian.exe") && ObsidianPDFMap
j:: Send "{Down}"
d:: Send "{PgDn}"
k:: Send "{Up}"
u::
+d:: Send "{PgUp}"
#HotIf
