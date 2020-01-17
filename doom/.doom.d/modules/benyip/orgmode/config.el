;;; benyip/orgmode/config.el -*- lexical-binding: t; -*-
(setq
 ;; one archive file instead of many
 org-directory "~/org/"
 org-archive-location
 (concat org-directory "archive.org::* From %s")
 org-log-done 'time
 org-log-done-with-time t
 org-bullets-bullet-list '("⁂")
 org-ellipsis (if (char-displayable-p ?⬎)  " ▾ " nil)
 org-startup-with-inline-images t)

(after! org-clock
  (setq
   org-clock-into-drawer t
   org-clock-out-remove-zero-time-clocks t))
