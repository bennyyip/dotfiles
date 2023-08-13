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
    if WinExist(title) {
        WinActivate title
    } else {
        RunWait fullpath
    }
}

Quote(s) {
    return '"' . s '"'
}
