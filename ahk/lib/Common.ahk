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
        return false
    } else {
        if WinExist(title) {
            WinActivate
        }
        else {
            RunWait fullpath
        }
        return true
    }
}

Quote(s) {
    return '"' . s '"'
}
