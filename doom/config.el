;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ben Yip"
      user-mail-address "yebenmy@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(if IS-LINUX
    (defconst benyip-font-spec (font-spec :family "monospace" :size 24))
  (defconst benyip-font-spec (font-spec :family "Sarasa Term CL Nerd" :size 20)))
(setq doom-font benyip-font-spec
      doom-variable-pitch-font benyip-font-spec
      doom-serif-font benyip-font-spec
      doom-unicode-font benyip-font-spec)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


(setq doom-theme 'sanityinc-tomorrow-night)

(after! color-theme-sanityinc-tomorrow
  (with-no-warnings
    (color-theme-sanityinc-tomorrow--with-colors
     'night
     (custom-set-faces
      `(cursor ((t (:background ,foreground))))
      `(show-paren-match ((t (:bold t :underline t :background ,term-black :foreground ,term-white))))))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defvar benyip-home-dir
  (if IS-WINDOWS
      (concat (getenv "USERPROFILE") "\\")
    (concat (getenv "HOME") "/")))

(setq doom-localleader-key "\\")

;;  don't mess with system clipboard
(setq select-enable-clipboard nil)

(setq show-trailing-whitespace t)

;; save buffers on focus lost
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

(global-set-key [remap list-buffers] 'ibuffer)

;; Enable `repeat-mode' to reduce key sequence length
;;
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
(use-package! repeat
  :custom
  (repeat-mode t)
  (repeat-exit-timeout 3)
  (repeat-exit-key (kbd "<escape>")))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
;; FIXME: 显示数字 not working
(use-package! hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                              hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))

(use-package! dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

;; Holidays
(use-package! calendar
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-chinese-all-holidays-flag t)
  (holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                            (holiday-fixed 3 12 "Arbor Day")
                            ,@(cl-loop for i from 1 to 3
                                       collect `(holiday-fixed 5 ,i "International Workers' Day"))
                            (holiday-fixed 5 4  "Chinese Youth Day")
                            (holiday-fixed 6 1  "Children's Day")
                            (holiday-fixed 9 10 "Teachers' Day")
                            ,@(cl-loop for i from 1 to 7
                                       collect `(holiday-fixed 10 ,i "National Day"))
                            (holiday-fixed 10 24 "Programmers' Day")
                            (holiday-fixed 11 11 "Singles' Day")))
  (holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
                            (holiday-fixed 4 23 "World Book Day")
                            (holiday-sexp '(if (or (zerop (% year 400))
                                                   (and (% year 100) (zerop (% year 4))))
                                               (list 9 12 year)
                                             (list 9 13 year))
                                          "World Programmers' Day")
                            (holiday-fixed 10 10 "World Mental Health Day")))
  (calendar-holidays `(,@holiday-general-holidays
                       ,@holiday-oriental-holidays
                       ,@holiday-christian-holidays
                       ,@holiday-other-holidays
                       ,@holiday-local-holidays))
  (calendar-mark-holidays-flag t)
  (calendar-mark-diary-entries-flag nil)
  ;; Prefer +0800 over CST
  (calendar-time-zone-style 'numeric)
  ;; year/month/day
  (calendar-date-style 'iso))

(use-package! lispy
  :config
  (map! :localleader
        :mode (list lisp-mode emacs-lisp-mode scheme-mode racket-mode)
        :n "l" #'lispy-mode))

(use-package! tree-sitter
  :config
  (setq +tree-sitter-hl-enabled-modes
        '(python-mode
          js-mode
          sh-mode
          markdown-mode)))

(after! geiser
  (geiser-implementation-extension 'guile "scm")
  (defun geiser-racket--language () '())
  (setq geiser-chez-binary "chez"))

(use-package! vdiff-magit
  :config
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

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

(use-package! lsp
  :config
  (setq lsp-enable-file-watchers 'nil))

;; :iabbrev
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)
(setq abbrev-file-name
      (concat doom-user-dir "abbrev_defs"))

;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq-default pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-large-scroll-height 40.0)
