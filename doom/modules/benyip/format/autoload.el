;;; benyip/format/config.el -*- lexical-binding: t; -*-

(defvar benyip/format-conf (make-hash-table))

;;;###autoload
(defun benyip/format-buffer ()
  (interactive)
  (let ((format-fns (gethash
                     major-mode
                     benyip/format-conf
                     '())))
    (mapc #'funcall format-fns)
    (doom/delete-trailing-newlines)
    (delete-trailing-whitespace)
    (whitespace-cleanup)))

;;;###autoload
(defun benyip/format-register (major--mode &rest fns)
  (puthash major--mode fns benyip/format-conf))
