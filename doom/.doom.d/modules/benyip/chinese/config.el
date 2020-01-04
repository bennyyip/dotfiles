;;; benyip/chinese/config.el -*- lexical-binding: t; -*-
(use-package! pyim
  :bind
  (("M-]" . pyim-convert-string-at-point)) ;与 pyim-probe-dynamic-english 配合
  :config
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  (map! :map 'pyim-mode-map
        "." #'pyim-page-next-page
        "," #'pyim-page-previous-page)

  (setq pyim-default-scheme 'rime)

  (setq default-input-method "pyim"
        pyim-page-length 9
        pyim-dicts
        `((:name
           "pyim-bigdict"
           :file
           ,(expand-file-name (concat doom-private-dir "etc/pyim/pyim-bigdict.pyim.gz")))))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 C-S-M-s-SPC 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1))

(use-package! liberime-config
  :init
  (setq liberime-user-data-dir (concat doom-private-dir "etc/rime"))
  (add-hook 'after-liberime-load-hook
    (lambda ()
      (liberime-select-schema "double_pinyin_flypy"))))
