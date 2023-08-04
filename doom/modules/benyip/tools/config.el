;;; benyip/tools/config.el -*- lexical-binding: t; -*-

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  ;; (setq keyfreq-excluded-commands
  ;;       '(abort-recursive-edit
  ;;         ace-window
  ;;         avy-goto-line
  ;;         clipboard-kill-ring-save
  ;;         comint-previous-input
  ;;         comint-send-input
  ;;         delete-backward-char
  ;;         describe-variable
  ;;         electric-pair-delete-pair
  ;;         eval-buffer
  ;;         exit-minibuffer
  ;;         ffip
  ;;         goto-line
  ;;         hippie-expand
  ;;         indent-new-comment-line
  ;;         ispell-minor-check
  ;;         js-mode
  ;;         js2-line-break
  ;;         kill-sentence
  ;;         left-char
  ;;         magit-next-line
  ;;         magit-previous-line
  ;;         markdown-exdent-or-delete
  ;;         markdown-outdent-or-delete
  ;;         minibuffer-complete
  ;;         minibuffer-complete-and-exit
  ;;         minibuffer-keyboard-quit
  ;;         move-beginning-of-line
  ;;         move-end-of-line
  ;;         mwheel-scroll
  ;;         my-company-number
  ;;         my-setup-develop-environment
  ;;         newline-and-indent
  ;;         next-history-element
  ;;         next-line
  ;;         package-menu-execute
  ;;         pcomplete
  ;;         previous-history-element
  ;;         previous-line
  ;;         push-button
  ;;         pwd
  ;;         quit-window
  ;;         recenter-top-bottom
  ;;         right-char
  ;;         rjsx-electric-gt
  ;;         rjsx-electric-lt
  ;;         self-insert-command
  ;;         shellcop-erase-buffer
  ;;         smarter-move-beginning-of-line
  ;;         suspend-frame
  ;;         term-send-raw
  ;;         turnon-keyfreq-mode
  ;;         typescript-insert-and-indent
  ;;         undefined ;;;  lambda function
  ;;         wgrep-finish-edit
  ;;         xterm-paste
  ;;         yank))
  ;; (setq keyfreq-excluded-regexp
  ;;       '("^ace-jump-"
  ;;         "^backward-"
  ;;         "^company-"
  ;;         "^dired"
  ;;         "^evil-"
  ;;         "^forward-"
  ;;         "^general-dispatch-self-insert-command-"
  ;;         "^gnus-"
  ;;         "^ido-"
  ;;         "^isearch-"
  ;;         "^ivy-"
  ;;         "^keyfreq-"
  ;;         "^keyboard-"
  ;;         "^my-hydra-.*/body"
  ;;         "^next-"
  ;;         "^org-"
  ;;         "^paredit-"
  ;;         "^save-"
  ;;         "^scroll-"
  ;;         "^select-window-"
  ;;         "^undo-"
  ;;         "^web-mode"
  ;;         "^w3m-"
  ;;         "^yas-"
  ;;         "^y-or-n-"
  ;;         "emms-"))
  )

(use-package! deadgrep)

