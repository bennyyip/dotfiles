#Requires AutoHotkey v2.0
#SingleInstance Force
#Include "utils.ahk"
GAME_TITLE := "ahk_exe eldenring.exe"

#HotIf WinActive("ahk_exe StreetFighter6.exe")

; 下左
q:: {
  KeyDown "s"
  Sleep 1 * TIME_PER_FRAME_60FPS
  KeyDown "a"
  Sleep 2 * TIME_PER_FRAME_60FPS
  KeyUp "s"
  Sleep 1 * TIME_PER_FRAME_60FPS
  KeyUp "a"

  ; KeyDown "s"
  ; Sleep 1 * TIME_PER_FRAME_60FPS
  ; KeyDown "a"
  ; Sleep 1 * TIME_PER_FRAME_60FPS
  ; KeyUp "s"
  ; Sleep 1 * TIME_PER_FRAME_60FPS
  ; KeyUp "a"
}

; 下右
e:: {
  KeyDown "s"
  Sleep 1 * TIME_PER_FRAME_60FPS
  KeyDown "d"
  Sleep 2 * TIME_PER_FRAME_60FPS
  KeyUp "s"
  Sleep 1 * TIME_PER_FRAME_60FPS
  KeyUp "d"

  ; KeyDown "s"
  ; Sleep 2 * TIME_PER_FRAME_60FPS
  ; KeyDown "d"
  ; Sleep 3 * TIME_PER_FRAME_60FPS
  ; KeyUp "s"
  ; Sleep 2 * TIME_PER_FRAME_60FPS
  ; KeyUp "d"
}
;
;
; ; 后重接打击投 左边
; 1:: {
;   KeyDown "a"
;   Sleep 2 * TIME_PER_FRAME_60FPS
;   KeyDown "i"
;   Sleep 3 * TIME_PER_FRAME_60FPS
;   KeyUp "a"
;   Sleep 3 * TIME_PER_FRAME_60FPS
;   Keyup "i"
;
;   KeyDown "s"
;   Sleep 1 * TIME_PER_FRAME_60FPS
;   KeyDown "d"
;   Sleep 1 * TIME_PER_FRAME_60FPS
;   KeyUp "s"
;   Sleep 1 * TIME_PER_FRAME_60FPS
;   KeyDown "y"
;   Sleep 3 * TIME_PER_FRAME_60FPS
;   Keyup "y"
;   KeyUp "d"
; }
; #HotIf
;
; ; 后重接打击投 右边
; 2:: {
;   KeyDown "d"
;   Sleep 2 * TIME_PER_FRAME_60FPS
;   KeyDown "i"
;   Sleep 3 * TIME_PER_FRAME_60FPS
;   KeyUp "d"
;   Sleep 3 * TIME_PER_FRAME_60FPS
;   Keyup "i"
;
;   KeyDown "s"
;   Sleep 1 * TIME_PER_FRAME_60FPS
;   KeyDown "a"
;   Sleep 1 * TIME_PER_FRAME_60FPS
;   KeyUp "s"
;   Sleep 1 * TIME_PER_FRAME_60FPS
;   KeyDown "y"
;   Sleep 3 * TIME_PER_FRAME_60FPS
;   Keyup "y"
;   KeyUp "a"
; }


#HotIf
