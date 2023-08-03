;;; benyip/windows/config.el -*- lexical-binding: t; -*-

(use-package! powershell
  :config
  (setq powershell-indent 2))

(setq shell-file-name "pwsh")


(add-to-list 'default-frame-alist '(fullscreen . maximized))
