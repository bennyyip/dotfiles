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
(setq doom-font (font-spec :family "Sarasa Term CL Nerd" :size 24)
      doom-unicode-font (font-spec :family "Sarasa Term CL Nerd"))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(if (display-graphic-p)
    (setq doom-theme 'doom-material-dark)
  (setq doom-theme 'whiteboard))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

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

(defvar my-home-dir
  (if IS-WINDOWS
      (concat (getenv "USERPROFILE") "\\")
    (concat (getenv "HOME") "/")))

(setq doom-localleader-key "\\")

(defun ghq-add-to-projectile ()
  (interactive)
  (mapc 'projectile-add-known-project (split-string (shell-command-to-string "ghq list -p") "\n")))

(defun alacritty-here ()
  (interactive "@")
  (shell-command
   (format
    "alacritty --working-directory %S > /dev/null 2>&1 & disown"
    default-directory)))

(use-package! tree-sitter
  :config
  (setq +tree-sitter-hl-enabled-modes
        '(python-mode
          js-mode
          sh-mode
          markdown-mode
          json-mode)))

(after! geiser
  (geiser-implementation-extension 'guile "scm")
  (setq geiser-chez-binary "chez"))

(use-package! server
  :config
  (if (display-graphic-p)
      (progn
        (setq server-name "gui")
        (server-start))
    (setq server-name "server")))

(defun geiser-racket--language () '())

(setq select-enable-clipboard nil)

;; :iabbrev
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)
(setq abbrev-file-name
      "~/.doom.d/abbrev_defs")

;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq-default pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-large-scroll-height 40.0)

;; open in gvim
(defun open-in-vim ()
  (interactive)
  (doom-call-process "gvim" "--remote-silent-tab" buffer-file-name))
