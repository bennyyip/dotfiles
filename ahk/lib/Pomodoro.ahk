Pomodoro(reset := false) {
    ; State: BreakEnd -> Work -> JobDone -> Break -> BreakEnd
    ; take a long break at every fourth break.

    WorkTime := 52
    ShortBreakTime := 17
    longBreakTime := 17

    static state := "BreakEnd"
    static beginTime := A_Now
    static breakTimes := 0
    static isLongBreak := false

    sec := -1000
    minute := sec * 60

    if (reset) {
        state := "BreakEnd"
        breakTimes := 0
        MsgBox "Reset!", "Pomodoro"
    } else if (state = "BreakEnd") {
        state := "Work"
        beginTime := A_now
        noti "Work starts"
        SetTimer PomodoroWorkEnd, minute * WorkTime
    } else if (state = "Work") {
        MsgBox(DateDiff(A_Now, beginTime, "Minutes") . "/" . WorkTime, "Pomodoro Work")
    } else if (state = "WorkEnd") {
        state := "Break"
        beginTime := A_now
        noti "Break starts"
        m := ShortBreakTime
        if (Mod(breakTimes, 4) = 3) {
            m := longBreakTime
            isLongBreak := true
        } else {
            isLongBreak := false
        }
        SetTimer PomodoroBreakEnd, minute * m
    } else if (state = "Break") {
        m := ShortBreakTime
        if isLongBreak
            m := longBreakTime

        MsgBox(DateDiff(A_Now, beginTime, "Minutes") . "/" . m, "Pomodoro Break")
    }

    PomodoroWorkEnd() {
        noti "Time for a break!"
        state := "WorkEnd"
    }

    PomodoroBreakEnd() {
        msg := "Time to focus!"
        KDE_CONNECT.Ping(msg)
        noti msg
        state := "BreakEnd"
        breakTimes += 1
    }

    noti(msg) {
        MsgBox msg, "Pomodoro"
        TrayTip msg
    }

}
