ShowDir(path) {
    Explorer_NewTab(path)
}

ActivateOrMinimize(title) {
  if WinActive(title) {
    WinMinimize title
    return true
  } else if WinExist(title) {
    WinActivate title
    return true
  }

  return false
}

Launch(fullpath) {
    SplitPath fullpath, &exe_name
    title := "ahk_exe " . exe_name

    if !ActivateOrMinimize(title) {
        Run fullpath
    }
}

Quote(s) {
    return '"' . s '"'
}

Explorer_NewTab(path) {
    ; ExplorerHwnd := WinExist("ahk_class CabinetWClass",,, "Address: Control Panel")
    ExplorerHwnd := WinExist("ahk_class CabinetWClass")

    if !ExplorerHwnd {
        Explorer_NewWindow(path)
        return
    }

    if path = "" {
        ActivateOrMinimize(ExplorerHwnd)
        return
    }

    if WinGetMinMax(ExplorerHwnd) = -1
        WinRestore(ExplorerHwnd)

    Windows := ComObject("Shell.Application").Windows
    Count := Windows.Count() ; Count of open windows


    ; open a new tab (https://stackoverflow.com/a/78502949)
    SendMessage(0x0111, 0xA21B, 0, "ShellTabWindowClass1", ExplorerHwnd)

    timeout := A_TickCount + 5000
    ; Wait for new tab.
    while Windows.Count() = Count {
        sleep 10
        ; If unable to create new tab in 5 seconds, create new window.
        if A_TickCount > timeout {
            Explorer_NewWindow(path)
            return
        }
    }
    Item := Windows.Item(Count)
    try Item.Navigate2(path) ; Navigate to path in new tab
    catch {
        Explorer_NewWindow(path)
    }
}

Explorer_NewWindow(path) {
    Run("Explorer " path)
    WinWaitActive("ahk_class CabinetWClass")
    ; SendEvent "{Space}" ; Select first item
}
