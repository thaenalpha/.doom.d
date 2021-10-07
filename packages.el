;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! arrayify
  :built-in 'prefer
  :recipe (:local-repo "lisp"
           :build (:not compile)))
;; ~/.doom.d/lisp/arrayify.el
(package! tldr)
(package! cheat-sh)
(package! howdoi)
