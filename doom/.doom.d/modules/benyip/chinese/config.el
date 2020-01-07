;;; benyip/chinese/config.el -*- lexical-binding: t; -*-
(use-package! pyim
  :init
  (setq pyim-title "ㄓ")
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
  ;; 根据探针决定全角半角
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1))

(use-package! liberime-config
  :init
  (setq liberime-user-data-dir (concat doom-private-dir "etc/rime"))
  (add-hook 'after-liberime-load-hook
    (lambda! (liberime-select-schema "double_pinyin_flypy"))))
