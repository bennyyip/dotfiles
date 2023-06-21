MyMenu := Menu()

MyMenu.Add "Paste into &HTML and Open",  ClipToBrowser
MyMenu.Add "Translate with &DeepL",  DeepL
MyMenu.Add "Translate with &Google Translate",  GoogleTranslate


ClipToBrowser(*) {
    f := FileOpen(A_Temp "\temp.html", "w")
    f.Write("<pre>" A_Clipboard "</pre>")
    f.Close()
    Run (A_Temp "\temp.html")
}

DeepL(*) {
    Run "https://www.deepl.com/translator#en/zh/" . A_Clipboard
}

GoogleTranslate(*) {
    Run "https://translate.google.com/?sl=auto&tl=zh-CN&op=translate&text=" . A_Clipboard
}
