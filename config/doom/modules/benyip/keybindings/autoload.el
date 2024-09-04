;;; benyip/keybindings/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun benyip/find-file-from-home ()
  (interactive)
  (doom-project-browse benyip-home-dir))
