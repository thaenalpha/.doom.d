;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! t)            ; unpin packages
;; Extend shr/eww with org features and analysis capability
(package! shrface)
(package! sql-indent)
(package! emacsql-mysql)
(package! emacs-with-nyxt
  :recipe (:host github :repo "ag91/emacs-with-nyxt"))

;;
;;; Tools

(package! dtache
  ;; Run shell cmds in sessions that are isolated from Emacs
  :recipe (:host gitlab :repo "niklaseklund/dtache"))
(unless IS-MAC
 (package! keychain-environment)
 (package! trashed))

;;
;;; UI

;;; Themes
(package! aj-dark+-theme
  :recipe (:host github :repo "AloisJanicek/.doom.d-2nd" :files ("themes/*dark+*")))
(package! doom-alabaster-theme
  :recipe (:host gitlab :repo "agraul/dotfiles" :files ("doom/themes/*alabaster*")))
(package! ahungry-theme)
(package! almost-mono-themes)

;;; Miscellaneous

;;
;;; Modules

;;; :completion company +tabnine
(when (featurep! :completion company +tabnine)
  (package! company-tabnine
    :recipe (:host github :repo "tommyX12/company-tabnine")))

;;; :editor evil +tree-sitter
(when (and (featurep! :editor evil +tree-sitter)
           (featurep! :tools tree-sitter))
  (package! evil-textobj-tree-sitter
    :recipe (:host github
             :repo "meain/evil-textobj-tree-sitter"
             :files (:defaults "queries"))))

;;; :emacs dired +dirvish
(when (featurep! :emacs dired +dirvish)
  (package! dirvish))

;;; :tools lookup +devdocs 
(when (featurep! :tools lookup +devdocs) (package! devdocs))

;;; :tools lookup +docsets
(when (and IS-MAC (featurep! :tools lookup +docsets)) (package! dash-at-point))

;;; :tools magit +forge
(when (and EMACS29+ (featurep! :tools magit +forge))
 (package! emacsql-sqlite-builtin
  :recipe (:host github :repo "tarsiiformes/emacsql"
           :branch "sqlite-backends")))

;;; :lang clojure
(when (featurep! :lang clojure) (package! clj-deps-new))

;;; :lang org
(package! orca)       ; Org Capture
(package! org-modern)
(package! org-roam-ui)
(when (featurep! :lang org +web) (package! org-web-tools))

;;; :lang web +tailwind
(when (and (featurep! :tools lsp) (featurep! :lang web +tailwind))
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))
