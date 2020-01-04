;;; benyip/chinese/config.el -*- lexical-binding: t; -*-
(use-package! pyim
  :bind
  :config
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  (map! :map 'pyim-mode-map
        "." #'pyim-page-next-page
        "," #'pyim-page-previous-page)

  (setq pyim-default-scheme 'rime)

  (setq default-input-method "pyim"
        pyim-page-length 9)

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1))

(use-package! liberime-config
  :init
  (setq liberime-user-data-dir (concat doom-private-dir "etc/rime"))
  (add-hook 'after-liberime-load-hook
    (lambda ()
      (liberime-select-schema "double_pinyin_flypy"))))
