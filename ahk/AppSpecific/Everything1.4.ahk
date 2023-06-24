class EverythingLegacy {
    static searchExecutabe() {
        ControlChooseString "Executable", "ComboBox1"
        Send "+^R" ; sort by run count
    }
    static searchEverything() {
        controlchoosestring "Everything", "ComboBox1"
        Send "^6" ; sort by mtime
    }
    static searchFolder() {
        ControlChooseString "Folder", "ComboBox1"
        Send "^6" ; sort by mtime
    }
}

#F:: {
    Run "C:\Program Files\Everything\Everything.exe"
    WinWaitActive "ahk_class EVERYTHING"
    EverythingLegacy.searchEverything
}
#+F:: {
    Run "C:\Program Files\Everything\Everything.exe"
    WinWaitActive "ahk_class EVERYTHING"
    EverythingLegacy.searchExecutabe
}

#HotIf WinActive("ahk_class EVERYTHING")

; Open file in vim, dir in terminal
+Enter:: {
    row := ListViewGetContent("Focused", "SysListView321")
    parts := StrSplit(row, "`t")
    fullpath := Quote(parts[2] . '\' . parts[1])
    entryType := parts[4]
    if (entryType = "File Folder") {
        Run "wt new-tab -d " . fullpath
    } else {
        Run EDITOR . fullpath
    }
}

; Run shell in parent dir
+^Enter:: {
    row := ListViewGetContent("Focused", "SysListView321")
    parts := StrSplit(row, "`t")
    Run "wt new-tab -d " . Quote(parts[2])
}

; Copy parent path
CapsLock & C::
^C:: {
    row := ListViewGetContent("Focused", "SysListView321")
    parts := StrSplit(row, "`t")
    parentPath := parts[2]
    A_Clipboard := parentPath
}

; Run
!R:: EverythingLegacy.searchExecutabe

; All
!A:: EverythingLegacy.searchEverything

; Dir
!D:: EverythingLegacy.searchFolder

#HotIf
