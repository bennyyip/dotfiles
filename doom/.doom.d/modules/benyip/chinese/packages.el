;; -*- no-byte-compile: t; -*-
;;; benyip/chinese/packages.el

(package! liberime-config
    :recipe (
              :host github
              :repo "merrickluo/liberime"
              :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))
(package! cal-china-x)
(package! posframe)
(package! sdcv :recipe (:host github :repo "manateelazycat/sdcv" :files ("sdcv.el")))
