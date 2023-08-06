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

  (when (modulep! :benyip format)
    (map!
     :leader
     :n "=" #'benyip/format-buffer))

  (map!
   :n "-" #'dired-jump                  ; like dirvish
   :n "gb" #'switch-to-buffer
   :n "T" #'+workspace/new
   :n "gs" #'delete-window              ; TODO: map avy and easymotion

   :n "[q" #'previous-error
   :n "]q" #'next-error

   :n [?\C-?] #'evil-ex-nohighlight

   (:prefix "C-w"
    :n "o" #'doom/window-maximize-buffer) ; instead of doom/window-enlargen

   :nv "gX" #'browse-url

   :v "." (kbd ":norm . RET")
   :v "Q" (kbd ":norm @q RET")
   :v (kbd "RET") #'align-regexp ; vim-easy-align. press number before get interactive mode

   :i "C-v" #'clipboard-yank            ; yank system clipboard

   :textobj "e" #'+evil:whole-buffer-txtobj #'+evil:whole-buffer-txtobj

   ;; TODO: yop toggle
   :n "]p" (lambda () ())
   ;; reset after evil-insert-state-exit-hook
   ;; (add-hook HOOK FUNCTION &optional DEPTH LOCAL)
   ;; LOCAL non-nil means modify buffer local
   ;; (remove-hook HOOK FUNCTION &optional LOCAL)


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
    :n "`" #'benyip/find-file-from-home))

  (define-key evil-ex-search-keymap (kbd "C-v") (general-simulate-key "C-r \""))
  (define-key evil-ex-completion-map (kbd "C-v") (general-simulate-key "C-r \""))

  (define-key evil-normal-state-map (kbd "'") (general-simulate-key "C-w"))
  (global-set-key [remap list-buffers] 'ibuffer))
