;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! t)                              ; Unpin packages

;;
;;; Completions
(when (featurep! :completion company +tabnine)
  (package! company-tabnine
    :recipe (:host github :repo "tommyX12/company-tabnine")))
;;
;;; Tools
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
(package! trashed)
;;
;;; Themes
(package! alabaster
  :recipe (:host github :repo "wandersoncferreira/alabaster-theme"))
(package! quartz
  :recipe (:host github :repo "fm0xb/quartz-theme.el"))
(package! moe-theme)
(package! gotham-theme)

;;; UI
(package! info-colors)
