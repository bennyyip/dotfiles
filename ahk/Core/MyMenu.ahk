MyMenu := Menu()

MyMenu.Add "Paste into &HTML and Open", MyMenuItems.ClipToBrowser
MyMenu.Add "Translate with &DeepL", MyMenuItems.DeepL
MyMenu.Add "Translate with &Google Translate", MyMenuItems.GoogleTranslate
MyMenu.Add "Open win MP&V", MyMenuItems.MPV
MyMenu.Add "&Paste to Vim", MyMenuItems.PasteToVim

killMenu := Menu()
killMenu.Add "&All explorers", MyMenuItems.KillAllExplorers
killMenu.Add "&Duplicate explorers", MyMenuItems.KillDuplicateExplorers
MyMenu.Add "&Kill", killMenu


class MyMenuItems {
    static ClipToBrowser(*) {
        f := FileOpen(A_Temp "\temp.html", "w")
        f.Write("<pre>" A_Clipboard "</pre>")
        f.Close()
        Run (A_Temp "\temp.html")
    }

    static DeepL(*) {
        Run "https://www.deepl.com/translator#en/zh/" . A_Clipboard
    }

    static GoogleTranslate(*) {
        Run "https://translate.google.com/?sl=auto&tl=zh-CN&op=translate&text=" . A_Clipboard
    }

    static MPV(*) {
        Run "mpv " . A_Clipboard
    }

    static KillAllExplorers(*) {
        wins := WinGetList("ahk_class CabinetWClass")
        subMenu := Menu()
        for w in wins {
            try {
                WinClose w
            }
        }
    }

    static KillDuplicateExplorers(*) {
        ws := WinGetList("ahk_class CabinetWClass")
        winSet := Map()
        for w in ws {
            title := WinGetTitle(w)
            if winSet.Has(title) {
                try {
                    WinClose w
                }
            } else {
                winSet[title] := 1
            }
        }
    }

    static PasteToVim(*) {
        RunWait "gvim --remote-send :Capture<CR>"
    }

}
