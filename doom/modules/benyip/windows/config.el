;;; benyip/windows/config.el -*- lexical-binding: t; -*-

(use-package! powershell
  :config
  (setq powershell-indent 2))

;; https://learn.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
(setq shell-file-name "pwsh")
(setq explicit-pwsh-args '("-Command" "-" ))
