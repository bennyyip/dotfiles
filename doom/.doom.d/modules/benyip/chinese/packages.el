;; -*- no-byte-compile: t; -*-
;;; benyip/chinese/packages.el

(package! liberime-config
    :recipe (
              :host github
              :repo "merrickluo/liberime"
              :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))

(package! posframe)
