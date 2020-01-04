;;; benyip/keybindings/config.el -*- lexical-binding: t; -*-

(setq evil-snipe-scope 'visible)
(setq evil-want-Y-yank-to-eol 't)

(map!
  :n "-" 'dired-jump ; like dirvish
  :n "gp" "`[v`]" ; select the previously pasted text
  :n "q" "@q" ; run the macro in the q register

 :map evil-window-map
 "o" 'doom/window-maximize-buffer) ; close other windows like vim

(define-key evil-normal-state-map (kbd "'") (general-simulate-key "C-w"))
