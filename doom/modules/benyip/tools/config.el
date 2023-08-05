;;; benyip/tools/config.el -*- lexical-binding: t; -*-

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(ace-window
          avy-goto-line
          clipboard-kill-ring-save
          comint-previous-input
          comint-send-input
          delete-backward-char
          describe-variable
          electric-pair-delete-pair
          eval-buffer
          exit-minibuffer
          ffip
          goto-line
          hippie-expand
          indent-new-comment-line
          ispell-minor-check
          js-mode
          js2-line-break
          kill-sentence
          left-char
          magit-next-line
          magit-previous-line
          markdown-exdent-or-delete
          markdown-outdent-or-delete
          minibuffer-complete
          minibuffer-complete-and-exit
          minibuffer-keyboard-quit
          move-beginning-of-line
          move-end-of-line
          mwheel-scroll
          my-company-number
          my-setup-develop-environment
          newline-and-indent
          next-history-element
          next-line
          package-menu-execute
          pcomplete
          previous-history-element
          previous-line
          push-button
          pwd
          quit-window
          recenter-top-bottom
          right-char
          rjsx-electric-gt
          rjsx-electric-lt
          self-insert-command
          shellcop-erase-buffer
          smarter-move-beginning-of-line
          suspend-frame
          term-send-raw
          turnon-keyfreq-mode
          typescript-insert-and-indent
          undefined ;;;  lambda function
          vertico-next
          wgrep-finish-edit
          xterm-paste
          yank
          abort-recursive-edit))
  (setq keyfreq-excluded-regexp
        '("^backward-"
          "^company-"
          "^dired"
          "^evil-"
          "^forward-"
          "^general-dispatch-self-insert-command-"
          "^gnus-"
          "^ido-"
          "^isearch-"
          "^ivy-"
          "^keyboard-"
          "^keyfreq-"
          "^lispy-"
          "^lispyville-"
          "^my-hydra-.*/body"
          "^next-"
          "^org-"
          "^paredit-"
          "^save-"
          "^scroll-"
          "^select-window-"
          "^special-lispy-"
          "^undo-"
          "^w3m-"
          "^web-mode"
          "^y-or-n-"
          "^yas-"
          "emms-"
          "^ace-jump-")))

(use-package! deadgrep)

;; open in gvim
(defun open-in-vim ()
  (interactive)
  (doom-call-process "gvim" "--remote-silent-tab" buffer-file-name))

;; open in VS Code
(defun open-in-vscode ()
  (interactive)
  (doom-call-process "code" buffer-file-name))

(defun ghq-add-to-projectile ()
  (interactive)
  (mapc 'projectile-add-known-project (split-string (shell-command-to-string "ghq list -p") "\n")))

(defun alacritty-here ()
  (interactive "@")
  (shell-command
   (format
    "alacritty --working-directory %S > /dev/null 2>&1 & disown"
    default-directory)))
