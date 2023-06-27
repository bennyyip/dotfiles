MyMenu := Menu()

MyMenu.Add "Paste into &HTML and Open", MyMenuItems.ClipToBrowser
MyMenu.Add "Translate with &DeepL", MyMenuItems.DeepL
MyMenu.Add "Translate with &Google Translate", MyMenuItems.GoogleTranslate
MyMenu.Add "Open win MP&V", MyMenuItems.MPV


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
}
