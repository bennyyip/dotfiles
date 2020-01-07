;;; benyip/config.el -*- lexical-binding: t; -*-


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Ben Yip"
      user-mail-address "bennyye@protonmail.com")

(setq doom-localleader-key "\\")

(setq projectile-project-search-path '("~/tencent"))

(defun ghq-add-to-projectile ()
  (interactive)
  (mapc 'projectile-add-known-project (split-string (shell-command-to-string "ghq list -p") "\n")))
