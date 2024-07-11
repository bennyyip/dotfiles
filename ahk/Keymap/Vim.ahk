global VimNormalMapping := false

CapsLock & Esc:: {
  global VimNormalMapping := !VimNormalMapping
  if (VimNormalMapping) {
    TraySetIcon "gvim.exe"
  } else {
    TraySetIcon A_AhkPath
  }
}

#HotIf VimNormalMapping
h:: Send "{Left}"
j:: Send "{Down}"
k:: Send "{Up}"
l:: Send "{Right}"
d:: Send "{PgDn}"
u::
+d:: Send "{PgUp}"
g:: Send "{Home}"
+g:: Send "{End}"
#HotIf
