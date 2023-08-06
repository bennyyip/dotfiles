;;; benyip/tools/autoload.el -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

;; open in gvim
;;;###autoload
(defun open-in-vim (&optional ARGS)
  (interactive)
  (if (display-graphic-p)
      (doom-call-process "gvim" "--remote-silent-tab" buffer-file-name ARGS)
    (progn
      (emamux:run-command (concat "sleep 0.1 && vim " (shell-quote-argument buffer-file-name) " " ARGS)) ; sleep 0.1 to avoid glitch
      (emamux:zoom-runner))))

;;;###autoload
(defun fugitive ()
  (interactive)
  (open-in-vim "+Git"))

;; open in VS Code
;;;###autoload
(defun open-in-vscode ()
  (interactive)
  (doom-call-process "code" buffer-file-name))

;;;###autoload
(defun ghq-add-to-projectile ()
  (interactive)
  (mapc 'projectile-add-known-project (split-string (shell-command-to-string "ghq list -p") "\n")))

;;;###autoload
(defun alacritty-here ()
  (interactive "@")
  (shell-command
   (format
    "alacritty --working-directory %S > /dev/null 2>&1 & disown"
    default-directory)))

;;; tldr
(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/%s/%s.md")

;; Silence compile warnings
(defvar url-http-end-of-headers)

;;;###autoload
(defun tldr (cmd &optional op)
  "View tldr page of CMD.
If OP is non-nil and search failed, OP will be used as platform
name and search again. Typically OP is nil or \"common\"."
  (interactive "sCommand: ")
  (let* ((platform (or op
                       (pcase system-type
                         ('gnu "linux")
                         ('gnu/linux "linux")
                         ('darwin "osx")
                         ('ms-dos "windows"))))
         (url (format tldr-url-template platform cmd)))
    (url-retrieve url
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (if (not op)
                            (tldr cmd "common")
                          (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                      (goto-char url-http-end-of-headers)
                      (let* ((req (json-read))
                             (encoding (alist-get 'encoding req))
                             (content (alist-get 'content req)))
                        (cl-assert (string= encoding "base64"))
                        (let ((buf (get-buffer-create tldr-buffer-name))
                              (inhibit-read-only t))
                          (with-current-buffer buf
                            (erase-buffer)
                            (insert (base64-decode-string content))
                            (when (functionp 'markdown-mode)
                              (markdown-mode))
                            (view-mode 1)
                            (pop-to-buffer buf)))))))))
