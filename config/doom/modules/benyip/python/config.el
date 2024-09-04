;;; benyip/python/config.el -*- lexical-binding: t; -*-


;; (use-package! flycheck
;;   :if (executable-find "ruff")
;;   :config

;;   (flycheck-define-checker python-ruff
;;     "A Python syntax and style checker using the ruff utility.
;; To override the path to the ruff executable, set
;; `flycheck-python-ruff-executable'.
;; See URL `http://pypi.python.org/pypi/ruff'."
;;     :command ("ruff"
;;               "--format=text"
;;               (eval (when buffer-file-name
;;                       (concat "--stdin-filename=" buffer-file-name)))
;;               "-")
;;     :standard-input t
;;     :error-filter (lambda (errors)
;;                     (let ((errors (flycheck-sanitize-errors errors)))
;;                       (seq-map #'flycheck-flake8-fix-error-level errors)))
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ":" (message) line-end))
;;     :modes python-mode)

;;   (defun flycheck-setup-python-ruff-h () (setq-local flycheck-checker 'python-ruff))
;;   (add-to-list 'flycheck-checkers 'python-ruff)
;;   (add-hook 'python-mode-hook #'flycheck-setup-python-ruff-h)
;;   )


(use-package! reformatter
  :config
  (reformatter-define black :program "black" :args '("-"))
  (reformatter-define isort :program "black" :args '("-"))
  (reformatter-define ruff :program "ruff" :args '("--fix" "-"))

  (benyip/format-register 'python-mode #'black-buffer #'ruff-buffer #'isort-buffer))


(use-package! python
  :if (executable-find "ipython")
  :config
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl))
