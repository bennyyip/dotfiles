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
        MsgBox "Work starts", "Pomodoro"
        SetTimer PomodoroWorkEnd, minute * WorkTime
    } else if (state = "Work") {
        MsgBox(DateDiff(A_Now, beginTime, "Minutes") . "/" . WorkTime, "Pomodoro Work")
    } else if (state = "WorkEnd") {
        state := "break"
        beginTime := A_now
        MsgBox "Break starts", "Pomodoro"
        m := ShortBreakTime
        if (Mod(breakTimes, 4) = 3) {
            m := longBreakTime
            isLongBreak := true
        } else {
            isLongBreak := false
        }
        SetTimer PomodoroBreakEnd, minute * 5
    } else if (state = "Break") {
        m := ShortBreakTime
        if isLongBreak
            m := longBreakTime

        MsgBox(DateDiff(A_Now, beginTime, "Minutes") . "/" . m, "Pomodoro Break")
    }

    PomodoroWorkEnd() {
        TrayTip "Time for a break!"
        state := "WorkEnd"
    }

    PomodoroBreakEnd() {
        TrayTip "Time to focus!"
        state := "BreakEnd"
        breakTimes += 1
    }

}
