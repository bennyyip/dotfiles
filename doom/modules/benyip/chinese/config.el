;;; benyip/chinese/config.el -*- lexical-binding: t; -*-


;; legacy config
;; https://github.com/bennyyip/dotfiles/blob/400b28677e39a0782a87300e360e6ff476161e58/doom/.doom.d/modules/benyip/chinese/config.el


(use-package! pyim
  :init
  (setq pyim-title "ㄓ")
  (let ((user-dict (concat doom-user-dir "private/ben.pyim")))
    (when (file-exists-p user-dict)
      (setq pyim-dicts (list (list :name "ben" :file user-dict)))))

  :bind (("M-]" . pyim-convert-string-at-point))
  :config
  (require 'pyim-basedict)


  (setq pyim-page-tooltip '(posframe popup minibuffer))
  (pyim-default-scheme 'xiaohe-shuangpin)

  (pyim-basedict-enable)

  (defun pyim-setup-h () (pyim-restart-1 t))
  (add-hook 'emacs-startup-hook #'pyim-setup-h)

  ;; 设置 pyim 探针
  ;; ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; ;; 我自己使用的中英文动态切换规则是：
  ;; ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; ;; 3. 使用 M-] 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  pyim-probe-evil-normal-mode
                  pyim-probe-org-speed-commands
                  ;; detect if current point is at button
                  (lambda () (button-at (point)))))



  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation
                  +chinese/pyim-probe-punctuation-after-english-letter))

  ;; 开启代码搜索中文功能（比如拼音，五笔码等）
  (pyim-isearch-mode 1))


;; https://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
