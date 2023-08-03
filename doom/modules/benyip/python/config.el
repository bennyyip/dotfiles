;;; benyip/python/config.el -*- lexical-binding: t; -*-


(use-package! flycheck
  :config

  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-ruff)
  (when (executable-find "ruff")
    (add-hook 'python-mode-hook (lambda () (setq-local flycheck-checker 'python-ruff)))))


(use-package! reformatter
  :config
  (reformatter-define black :program "black" :args '("-"))
  (reformatter-define isort :program "black" :args '("-"))
  (reformatter-define ruff :program "ruff" :args '("--fix" "-"))

  (when (modulep! :benyip format)
    (benyip/format-register 'python-mode #'black-buffer #'ruff-buffer #'isort-buffer)))
