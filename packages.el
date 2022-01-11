;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! t)                              ; Unpin packages

;;
;;; Tools

(package! debian-el)
(package! dtache
  ;; run shell cmds in sessions that are isolated from Emacs
  :recipe (:host gitlab :repo "niklaseklund/dtache"))
(package! keychain-environment)
(package! trashed)

;;
;;; UI

;;; Miscellaneous
(package! info-colors)

;;
;;; Modules

;;; :completion company +tabnine
(when (featurep! :completion company +tabnine)
  (package! company-tabnine
    :recipe (:host github :repo "tommyX12/company-tabnine")))

;;; :completion vertico +childframe
(when (featurep! :completion vertico +childframe)
  (package! vertico-posframe))

;;; :editor evil +tree-sitter
(when (and (featurep! :editor evil +tree-sitter)
           (featurep! :tools tree-sitter))
  (package! evil-textobj-tree-sitter
    :recipe (:host github
             :repo "meain/evil-textobj-tree-sitter"
             :files (:defaults "queries"))))

;;; :tools magit +forge
(when (and EMACS29+ (featurep! :tools magit +forge))
 (package! emacsql-sqlite-builtin
  :recipe (:host github :repo "tarsiiformes/emacsql" :branch "sqlite-backends")))

;;; :lang org
(package! org-ref)

;;; :lang web +tailwind
(when (and (featurep! :tools lsp) (featurep! :lang web +tailwind))
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))
