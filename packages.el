;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; Unpin packages
(unpin! t)

;;; Completions
(when (featurep! :completion company +tabnine)
  (package! company-tabnine
    :recipe (:host github :repo "tommyX12/company-tabnine")))

;;; Tools
(package! github-search)
(package! keychain-environment)
(when (and (featurep! :tools lsp) (featurep! :lang web +tailwind))
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))
(when (and (featurep! :editor evil +tree-sitter)
           (featurep! :tools tree-sitter))
  (package! evil-textobj-tree-sitter
    :recipe (:host github
             :repo "meain/evil-textobj-tree-sitter"
             :files (:defaults "queries"))))
(package! debian-el)
(package! dtache ; run shell cmds in sessions that are isolated from Emacs
  :recipe (:host gitlab :repo "niklaseklund/dtache"))
(package! fixmee)

;;; Themes

;;; UI
(package! info-colors)
