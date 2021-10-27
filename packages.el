;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! arrayify
  :recipe (:local-repo "lisp"))
;; ~/.doom.d/lisp/arrayify.el
(package! tldr)
(package! cheat-sh) ; tldr but more detail, weaker syntax highlight
;; (package! howdoi) Not working now, I almost fixed it, but I have mistaken lost it when I upgrade Doom. Due to I edit src in straight package repo and Doom enforcing to discard or stash changes before upgrade and then my changes were gone.
(package! github-search)
;; (package! lsp-tailwindcss)
;; (package! exec-path-from-shell) ensure environment variables inside Emacs look the same as in the user's shell.
