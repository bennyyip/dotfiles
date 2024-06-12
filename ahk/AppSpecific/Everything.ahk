class Everything {
    static exe := 'C:\Program Files\Everything 1.5a\Everything64.exe'

    static searchExecutabe() {
        Run this.exe . ' -filter executable -sort "run count" -sort-descending'
    }
    static searchEverything() {
        Run this.exe . ' -filter everything -sort "Date Modified" -sort-descending'
    }
    static searchFolder() {
        Run this.exe ' -filter Folder -sort "Date Modified" -sort-descending'
    }

    ; Open file in vim, dir in terminal
    static OpenInVimOrTerminal() {
        row := ListViewGetContent("Focused", "SysListView321", "ahk_class EVERYTHING_(1.5a)")
        parts := StrSplit(row, "`t")
        fullpath := Quote(parts[2] . '\' . parts[1])
        entryType := parts[4]
        if (entryType = "File Folder") {
            Run "wt new-tab -d " . fullpath
        } else {
            Run EDITOR . fullpath
        }
    }

    ; Copy parent path
    static CopyParentPath() {
        row := ListViewGetContent("Focused", "SysListView321")
        parts := StrSplit(row, "`t")
        parentPath := parts[2]
        A_Clipboard := parentPath
    }

    ; Run terminal in parent dir
    static OpenParentInTerminal() {
        row := ListViewGetContent("Focused", "SysListView321")
        parts := StrSplit(row, "`t")
        Run "wt new-tab -d " . Quote(parts[2])
    }
}

#F:: Everything.searchEverything
#+F:: Everything.searchExecutabe

#HotIf WinActive("ahk_class EVERYTHING_(1.5a)")

+Enter:: Everything.OpenInVimOrTerminal
+^Enter:: Everything.OpenParentInTerminal

CapsLock & C::
^C:: Everything.CopyParentPath

; Run
!R:: Everything.searchExecutabe
; All
!A:: Everything.searchEverything
; Dir
!D:: Everything.searchFolder

#HotIf
