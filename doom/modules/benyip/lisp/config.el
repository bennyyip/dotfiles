;;; benyip/lisp/config.el -*- lexical-binding: t; -*-

(use-package! lispy
  :config
  (map!
   :localleader
   :map prog-mode-map
   :mode (list lisp-mode emacs-lisp-mode scheme-mode racket-mode)
   "l" #'lispy-mode))

(use-package! lispyville
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          ;; slurp/barf-lispy ;; save << >> for indent
          additional
          additional-insert)))


(after! geiser
  (geiser-implementation-extension 'guile "scm")
  (defun geiser-racket--language () '())
  (setq geiser-chez-binary "chez"))
