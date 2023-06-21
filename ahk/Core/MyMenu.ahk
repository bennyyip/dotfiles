MyMenu := Menu()

MyMenu.Add "Paste into html and open",  ClipToBrowser

ClipToBrowser(*) {
    f := FileOpen(A_Temp "\temp.html", "w")
    f.Write("<pre>" A_Clipboard "</pre>")
    f.Close()
    Run (A_Temp "\temp.html")
}

#z::MyMenu.Show  ; i.e. press the Win-Z hotkey to show the menu.
