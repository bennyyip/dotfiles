;;; benyip/keybindings/config.el -*- lexical-binding: t; -*-

(use-package! move-text
  :config
  (map!
   :n "[e" #'move-text-up
   :n "]e" #'move-text-down))


(use-package! clipetty
  :bind (("M-w" . clipetty-kill-ring-save))
  :config
  (map!
   :leader
   :v "c" #'clipetty-kill-ring-save))


(use-package! evil
  :config
  (setq evil-snipe-scope 'visible)
  (setq evil-want-Y-yank-to-eol 't)

  (keymap-set evil-ex-search-keymap "C-v" (general-simulate-key "C-r \""))
  (keymap-set evil-ex-completion-map "C-v" (general-simulate-key "C-r \""))
  (keymap-set evil-normal-state-map "'" (general-simulate-key "C-w"))

  (map!
   :n "-" #'dired-jump                  ; like dirvish
   :n "T" #'+workspace/new

   :n "[q" #'previous-error
   :n "]q" #'next-error

   :n [?\C-?] #'evil-ex-nohighlight

   (:prefix "C-w"
    :n "RET" (general-simulate-key "C-x 4") ; open in other window
    :n "'" #'other-window
    :n "o" #'doom/window-maximize-buffer) ; instead of doom/window-enlargen

   (:prefix "g"
    :n "?" #'describe-mode              ; TODO: map avy and easymotion
    :n "s" #'delete-window              ; TODO: map avy and easymotion
    :n "b" #'switch-to-buffer
    :nv "X" #'browse-url)

   :v "." (kbd ":norm . RET")
   :v "Q" (kbd ":norm @q RET")
   :v (kbd "RET") #'align-regexp ; vim-easy-align. press number before get interactive mode

   :i "C-v" #'clipboard-yank            ; yank system clipboard

   :textobj "e" #'+evil:whole-buffer-txtobj #'+evil:whole-buffer-txtobj

   :leader
   ;;  ;; (:prefix-map ("o" . "open")
   ;;  ;;              "c" (λ! (find-file "~/org/gtd/calendar.org"))
   ;;  ;;              "n" (λ! (find-file "~/org/notes.org"))
   ;;  ;;              "t" (λ! (find-file "~/org/todo.org")))

   :n "m" #'highlight-symbol-at-point
   :n "M" #'unhighlight-regexp

   :n "1" #'+workspace/switch-to-0
   :n "2" #'+workspace/switch-to-1
   :n "3" #'+workspace/switch-to-2
   :n "4" #'+workspace/switch-to-3
   :n "5" #'+workspace/switch-to-4
   :n "6" #'+workspace/switch-to-5
   :n "7" #'+workspace/switch-to-6
   :n "8" #'+workspace/switch-to-7
   :n "9" #'+workspace/switch-to-8
   :n "0" #'+workspace/switch-to-final

   (:prefix "f"
    :n "f" #'+default/find-file-under-here
    :n "`" #'benyip/find-file-from-home)))


;; HACK workaround for AltGr on Windows
(defconst +w32-altgr-map-list
  '(("ä" "C-M-a")
    ("í" "C-M-b")
    ("ç" "C-M-c")
    ("đ" "C-M-d")
    ("ë" "C-M-e")
    ("è" "C-M-f")
    ("é" "C-M-g")
    ("ù" "C-M-h")
    ("ï" "C-M-i")
    ("ú" "C-M-j")
    ("ĳ" "C-M-k")
    ("ø" "C-M-l")
    ("α" "C-M-m")
    ("ñ" "C-M-n")
    ("ö" "C-M-o")
    ("œ" "C-M-p")
    ("æ" "C-M-q")
    ("ý" "C-M-r")
    ("ß" "C-M-s")
    ("þ" "C-M-t")
    ("ü" "C-M-u")
    ("ì" "C-M-v")
    ("å" "C-M-w")
    ("á" "C-M-x")
    ("ÿ" "C-M-y")
    ("à" "C-M-z")))

(defun +w32-map-altgr-keys ()
  (interactive)
  (mapc
   (lambda (x) (apply #'keymap-set key-translation-map x))
   +w32-altgr-map-list))

(defun +w32-unmap-altgr-keys ()
  (interactive)
  (mapc
   (lambda (x) (keymap-unset key-translation-map (car x)))
   +w32-altgr-map-list))

(+w32-map-altgr-keys)
