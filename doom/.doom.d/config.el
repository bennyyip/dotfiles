;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(evil-define-key 'normal 'global
  ;; select the previously pasted text
  "gp" "`[v`]"
  ;; run the macro in the q register
  "Q" "@q")


(eval-after-load 'evil-core
                   '(evil-set-initial-state 'nov-mode 'emacs))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun my-nov-setup ()
  (blink-cursor-mode 0)
  (face-remap-add-relative 'variable-pitch :family "Source Han Serif K"
                                           :height 1.1))

(setq nov-text-width 60)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)
(add-hook 'nov-mode-hook 'my-nov-setup)


(setq org-agenda-files (list "~/org/todo.org"))
