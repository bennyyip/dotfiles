;;; benyip/chinese/config.el -*- lexical-binding: t; -*-
(use-package! pyim
  :commands pyim-convert-string-at-point
  :init
  (setq pyim-title "ㄓ")
  :bind
  (("M-]" . pyim-convert-string-at-point)) ;与 pyim-probe-dynamic-english 配合
  :config
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  (map! :map 'pyim-mode-map
        "." #'pyim-page-next-page
        "," #'pyim-page-previous-page)

  (if IS-LINUX
      (setq pyim-default-scheme 'rime)
    (setq pyim-default-scheme 'quanpin))

  (setq default-input-method "pyim"
        pyim-page-length 9)

  ;; 探针设置来自@SteamedFish
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 C-S-M-s-SPC 快捷键，强制将光标前的拼音字符串转换为中文。
  ;; 4. 当光标在按钮上时，切换到英文输入。
  ;; 开启拼音搜索功能
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  pyim-probe-evil-normal-mode
                  pyim-probe-org-speed-commands
                  ;; detect if current point is at button
                  (lambda () (button-at (point)))))

  (add-hook! 'prog-mode-hook
    (add-to-list 'pyim-english-input-switch-functions
                 'pyim-probe-dynamic-english))

  (add-hook! 'text-mode-hook
    (add-to-list 'pyim-english-input-switch-functions
                 'pyim-probe-auto-english))

  (add-hook! '(text-mode-hook prog-mode-hook)
             ;; active pyim by default
             ;; for some unknown reason directly calling `active-input-method'
             ;; is not working, but it works with `run-at-time'
             ((lambda () (run-at-time nil nil 'activate-input-method "pyim"))))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation
                  +chinese/pyim-probe-punctuation-after-english-letter))

  ;; pyim will reset all local variables in this list for some reason,
  ;; but we don't want some of them to be reseted everytime pyim is
  ;; activate of deactivate, so we can use different settings in
  ;; different buffers.
  ;; https://github.com/tumashu/pyim/issues/342
  (setq pyim-local-variable-list
        (delete 'pyim-english-input-switch-functions pyim-local-variable-list))

  (pyim-isearch-mode 1))

(use-package! liberime
  :when (or IS-LINUX IS-MAC)
  :init
  (setq liberime-user-data-dir (expand-file-name (concat doom-private-dir "/etc/rime/")))
                                        ; (when IS-LINUX
                                        ;   (setq liberime-shared-data-dir (expand-file-name "~/.config/fcitx/rime/")))
                                        ; (when IS-MAC
                                        ;   (setq liberime-shared-data-dir (expand-file-name "~/Library/Rime/")))
  (setq pyim-titles '("ㄓ" "ㄓ-EN" "ㄓ-AU"))
  (add-hook! 'after-init-hook
             #'liberime-sync)
  (add-hook! 'liberime-after-start-hook
    (lambda () (liberime-select-schema "double_pinyin_flypy")))
  :config
  (unless (file-exists-p (concat (liberime-get-library-directory)
                                 "build/liberime-core"
                                 module-file-suffix))
    (liberime-build)))


(use-package! sdcv
  :commands (sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-search-input
             sdcv-search-input+)
  :init
  (map!
   :leader
   (:prefix-map ("a" . "private")
    "s" #'sdcv-search-pointer
    "S" #'sdcv-search-pointer+
    "i" #'sdcv-search-input
    "I" #'sdcv-search-input+))

  :config
  (evil-set-initial-state 'sdcv-mode 'emacs)
  (setq sdcv-say-word-p nil
        sdcv-dictionary-data-dir (concat (xdg-config-home) "/stardict")))

(use-package! cal-china-x
  :after calendar
  :config
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        calendar-holidays (append
                           cal-china-x-important-holidays
                           cal-china-x-general-holidays)))
