;;; benyip/lisp/config.el -*- lexical-binding: t; -*-

(defconst +lispy-modes '(lisp-mode emacs-lisp-mode scheme-mode racket-mode))

(use-package! lispy
  :config
  (map!
   :localleader
   ;; TODO: How to use +lispy-modes here?
   :mode (lisp-mode emacs-lisp-mode scheme-mode racket-mode)
   "l" #'lispy-mode))

(use-package! lispyville
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify)
          (atom-movement t)
          ;; slurp/barf-lispy ;; save << >> for indent
          additional
          additional-insert)))

(defun +lispyville-prettify-buffer ()
  (interactive)
  ;; lispyville-prettify does not respect BEGIN, so gg first
  (save-excursion
    (goto-char (point-min))
    (lispyville-prettify (point-min) (point-max))))

(dolist (m +lispy-modes)
  (benyip/format-register m #'+lispyville-prettify-buffer))

(after! geiser
  (geiser-implementation-extension 'guile "scm")
  (defun geiser-racket--language () '())
  (setq geiser-chez-binary "chez"))
