;;; benyip/keybindings/config.el -*- lexical-binding: t; -*-

(use-package! move-lines
  :config
  (map!
   :n "[e" #'move-lines-up   ; unimpaired
   :n "]e" #'move-lines-down ; unimpaired
   ))

(setq evil-snipe-scope 'visible)
(setq evil-want-Y-yank-to-eol 't)

(map!
 :n "-" #'dired-jump ; like dirvish
 :n "gp" "`[v`]" ; select the previously pasted text
 :n "Q" "@q" ; run the macro in the q register
 :n "gb" #'ivy-switch-buffer
 :n "T"  #'+workspace/new
 :n "gs" #'+workspace/close-window-or-workspace

 :n "[q" #'previous-error ; good old quickfix
 :n "]q" #'next-error     ; good old quickfix

 :n "<backspace>" #'evil-ex-nohighlight

 :v "." (kbd ":norm . RET")
 :v "Q" (kbd ":norm @q RET")
 :v (kbd "RET") #'align-regexp ; vim-easy-align. press number before get interactive mode

 :i "C-v" #'evil-paste-after

 :leader
 (:prefix-map ("o" . "open")
   "c" (λ! (find-file "~/org/gtd/calendar.org"))
   "n" (λ! (find-file "~/org/notes.org"))
   "t" (λ! (find-file "~/org/todo.org")))

 :n "m" #'highlight-symbol-at-point
 :n "M" #'unhighlight-regexp

 :n "1"    #'+workspace/switch-to-0
 :n "2"    #'+workspace/switch-to-1
 :n "3"    #'+workspace/switch-to-2
 :n "4"    #'+workspace/switch-to-3
 :n "5"    #'+workspace/switch-to-4
 :n "6"    #'+workspace/switch-to-5
 :n "7"    #'+workspace/switch-to-6
 :n "8"    #'+workspace/switch-to-7
 :n "9"    #'+workspace/switch-to-8
 :n "0"    #'+workspace/switch-to-final)

(define-key evil-normal-state-map (kbd "'") (general-simulate-key "C-w"))
