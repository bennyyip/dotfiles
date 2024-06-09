ShowDir(title) {
   if WinExist(title . " ahk_class CabinetWClass") {
     WinActivate
   } else {
     Run title
   }
}

Launch(fullpath) {
  SplitPath fullpath, &exe_name
  title := "ahk_exe " . exe_name

  if WinActive(title) {
    WinMinimize title
  } else if WinExist(title) {
    WinActivate title
  } else {
    Run fullpath
  }
}

Quote(s) {
    return '"' . s '"'
}
