;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 24 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 25))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(edit-server-start)
(server-start)
(require 'org-protocol)
(setq org-capture-templates (append org-capture-templates
    '(("P" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
    ("l" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %? [[%:link][%:description]] \nCaptured On: %U")
    ("w" "Web site" entry (file+olp "~/org/inbox.org" "Web")
        "* %c :website:\n%U %?%:initial"))))

;; mu4e config
;; :ensure nil
;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
;; :defer 20 ; Wait until 20 seconds after startup

;; This is set to 't' to avoid mail syncing issues when using mbsync
(setq mu4e-change-filenames-when-moving t)

;; Refresh mail using isync every 10 minutes
(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/Mail")

(setq mu4e-drafts-folder "/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
(setq mu4e-refile-folder "/[Gmail]/All Mail")
(setq mu4e-trash-folder  "/[Gmail]/Trash")

(setq mu4e-maildir-shortcuts
      '(("/Inbox"             . ?i)
        ("/[Gmail]/Sent Mail" . ?s)
        ("/[Gmail]/Trash"     . ?t)
        ("/[Gmail]/Drafts"    . ?d)
        ("/[Gmail]/All Mail"  . ?a)))

(add-load-path! "~/.doom.d/lisp")
(require 'wsl)
