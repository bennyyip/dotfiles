;;; benyip/windows/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ;; https://github.com/jschaf/powershell.el/blob/f2da15857e430206e215a3c65289b4058ae3c976/powershell.el
;; (defun powershell (&optional buffer)
;;   "Run an inferior PowerShell.
;; If BUFFER is non-nil, use it to hold the powershell
;; process.  Defaults to *PowerShell*.

;; Interactively, a prefix arg means to prompt for BUFFER.

;; If BUFFER exists but the shell process is not running, it makes a
;; new shell.

;; If BUFFER exists and the shell process is running, just switch to
;; BUFFER.

;; See the help for `shell' for more details.  \(Type
;; \\[describe-mode] in the shell buffer for a list of commands.)"
;;   (interactive
;;    (list
;;     (and current-prefix-arg
;;          (read-buffer "Shell buffer: "
;;                       (generate-new-buffer-name "*PowerShell*")))))

;;   (setq buffer (get-buffer-create (or buffer "*PowerShell*")))
;;   (let ((explicit-shell-file-name "pwsh"))
;;     (shell buffer))

;;   (let ((proc (get-buffer-process buffer)))
;;     ;; disallow backspace over the prompt:
;;     (setq-local comint-prompt-read-only t)

;;     ;; hook the kill-buffer action so we can kill the inferior process?
;;     (add-hook 'kill-buffer-hook 'powershell-delete-process)

;;     ;; send a carriage-return  (get the prompt)
;;     ;; (comint-send-input)
;;     ;; (accept-process-output proc)
;;     )

;;   ;; return the buffer created
;;   buffer)

;; (defun powershell-delete-process (&optional proc)
;;   "Delete the current buffer process or PROC."
;;   (or proc
;;       (setq proc (get-buffer-process (current-buffer))))
;;   (and (processp proc)
;;        (delete-process proc)))
