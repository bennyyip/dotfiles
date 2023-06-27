class KDE_CONNECT {
    static CMD := "C:\Program Files\KDE Connect\bin\kdeconnect-cli.exe -n XQ-BQ72 "

    static Ping(msg) {
        Run this.CMD . "--ping-msg " . Quote(msg)
    }

}
