;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! arrayify
  :recipe (:host github :repo "thaenalpha/.doom.d"
           :files ("lisp/arrayify.el")))
;; ~/.doom.d/lisp/arrayify.el
(package! github-search)
(package! keychain-environment)
(package! lsp-tailwindcss 
  :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! info-colors)
