#Include "%A_ScriptDir%\lib\VDA.ahk"

ShowDir(title) {
    if WinExist(title . " ahk_class CabinetWClass") {
        WinActivate
    } else {
        Run title
    }
}

CloseDuplicateExplorerWindows() {
    ws := WinGetList("ahk_class CabinetWClass")
    winSet := Map()
    for w in ws {
        title := WinGetTitle(w)
        if winSet.Has(title) {
            WinClose w
        } else {
            winSet[title] := 1
        }
    }
}

PasteToVim() {
    draftFile := HOME_DIR . "/temp/" . FormatTime(, "yyyy-MM-dd") . ".txt"
    RunWait "gvim.exe --remote " . draftFile
    SendText 'G] j"+p'
}
