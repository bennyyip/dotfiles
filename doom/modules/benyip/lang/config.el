;;; benyip/lang/config.el -*- lexical-binding: t; -*-


(use-package! tree-sitter
  :config
  (setq +tree-sitter-hl-enabled-modes
        '(python-mode
          js-mode
          sh-mode
          markdown-mode)))

;; (use-package! lsp
;;   :config
;;   (setq lsp-enable-file-watchers 'nil))


;; https ://emacs-china.org/t/citre-ctags/17604
(use-package! citre
  :init
  ;; Load the prelude.
  (require 'citre-config)
  :bind (("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back)
         ("C-c c p" . citre-peek)
         ("C-c c a" . citre-ace-peek)
         ("C-c c u" . citre-update-this-tags-file))
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-prompt-language-for-ctags-command t))
