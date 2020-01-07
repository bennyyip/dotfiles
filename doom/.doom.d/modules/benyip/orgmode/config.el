;;; benyip/orgmode/config.el -*- lexical-binding: t; -*-
(setq
 ;; one archive file instead of many
 org-directory "~/org"
 org-archive-location
 (concat org-directory "archive.org::* From %s")
 org-log-done 'time
 org-log-done-with-time t
 org-bullets-bullet-list '("⁂")
 org-ellipsis (if (char-displayable-p ?⬎)  " ▾ " nil)
 org-startup-with-inline-images t)
