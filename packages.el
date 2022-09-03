;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Extend shr/eww with org features and analysis capability
(package! shrface)
(package! sql-indent)
(package! emacsql-mysql)
(package! emacs-with-nyxt
  :recipe (:host github :repo "ag91/emacs-with-nyxt"))
(package! nndiscourse)
(unless IS-MAC (package! cask))         ; in prefer of `Brew'
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! minibuffer-header
  :recipe (:host github :repo "rougier/minibuffer-header"))
(package! path-headerline-mode)

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
(package! aj-dark+-theme                ; vscode dark+ theme
  :recipe (:host github :repo "AloisJanicek/.doom.d-2nd"
           :files ("themes/*dark+*")))
(package! ahungry-theme)                ; Ahungry color theme for Emacs.
(package! almost-mono-themes)           ; almost monochromatic color themes
(package! agraul-doom-themes            ; doom-themes with alabaster & github
  :recipe (:host gitlab :repo "agraul/dotfiles"
           :files ("doom/themes/*.el")))
(package! quartz-theme
  ;; Dark Emacs theme inspired by Alabaster Dark & Jonathans Awesome Dark Theme
  :recipe (:host github :repo "fm0xb/quartz-theme.el"))

;;; Miscellaneous

;;
;;; Modules

;;; :completion company +tabnine
(when (featurep! :completion company +tabnine)
  (package! company-tabnine
    :recipe (:host github :repo "tommyX12/company-tabnine")))

;;; :tools lookup +devdocs 
(when (featurep! :tools lookup +devdocs) (package! devdocs))

;;; :tools lookup +docsets
(when (and IS-MAC (featurep! :tools lookup +docsets))
  (package! dash-at-point))

;;; :tools magit +forge
(when (and EMACS29+ (featurep! :tools magit +forge))
 (package! emacsql-sqlite-builtin
  :recipe (:host github :repo "tarsiiformes/emacsql"
           :branch "sqlite-backends")))

;;; :lang clojure
(when (featurep! :lang clojure) (package! clj-deps-new))

;;; :lang org
(when (featurep! :lang org)
  (package! orca)                       ; Org Capture
  (package! org-roam-ui)                ; User Interface for Org-roam
  (package! org-ol-tree
    :recipe (:host github :repo "Townk/org-ol-tree"))
  (package! org-appear                  ; Auto-toggle Org elements
    :recipe (:host github :repo "awth13/org-appear"))
  (package! org-modern)                 ; Modern Org Mode
  (package! org-transclusion)           ; Transclude text content via links
  (package! doct :recipe (:host github :repo "progfolio/doct"))
  (when (featurep! :lang org +web)
    (package! org-web-tools))           ; Display and capture web content
  (package! org-yt :recipe (:host github :repo "thaenalpha/org-yt"))
  (package! org-edit-indirect           ; Edit anything, not just src blocks
    :recipe (:host github :repo "agzam/org-edit-indirect.el")))

;;; :lang web +tailwind
(when (and (featurep! :tools lsp) (featurep! :lang web +tailwind))
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))

(unpin! :app :checkers :completion :config :editor
        :emacs :email :input :lang :os :term :tools :ui)
