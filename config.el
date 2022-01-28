;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; For e.g. GPG configuration, email clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")

;;
;;; System

(setq window-combination-resize t ; take new window space from all other windows
      truncate-string-ellipsis  "…")    ; Unicode ellispis are nicer than "...",
                                        ; and also save /precious/ space
(global-subword-mode 1)                 ; Iterate through CamelCase words

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(unless (equal "Battery status not available" (battery)) ; on laptops…
  (display-battery-mode 1)) ; it's nice to know how much power you have

(setq-default delete-by-moving-to-trash t) ; Delete files to trash

(setq byte-compile-warnings '(not cl-functions))

(use-package keychain-environment
  :defer t
  :init (keychain-refresh-environment))

(setq company-idle-delay nil)

;;
;;; UI

(setq display-line-numbers-type  nil
      +treemacs-git-mode         'deferred
      doom-themes-treemacs-theme 'doom-colors
      doom-font                  (font-spec :family "JetBrains Mono"
                                            :size 12 :weight 'light)
      doom-variable-pitch-font   (font-spec :family "Noto Serif"     :size 13)
      vertico-posframe-font      (font-spec :family "JetBrains Mono" :size 15)
      doom-unicode-font          (font-spec :family "FiraGO" :weight 'Book)
      doom-serif-font            doom-variable-pitch-font
      default-frame-alist        (append default-frame-alist
                                         '((height . 48)
                                           (width  . 160)
                                           (inhibit-double-buffering . t))))

(let ((my-doom-color 'auto))
  (eval                           ; theme varies to the value of `my-doom-color'
   `(let ((auto       'auto)
          (default    'doom-one)
          (light      'doom-one-light)
          (dark       'doom-dracula)
          (custom     'doom-vibrant))
      (if (eq ,my-doom-color 'auto)
          (run-with-timer
           0 3600                       ; check for every hour
           (defun synchronize-theme (light dark)
             "Sets the theme according to the hour in the current time.
If the hour is (both inclusive) in `light-theme-hours' then
`light' theme is loaded, otherwise `dark' theme is loaded."
             (let* ((hour (string-to-number
                           (substring (current-time-string) 11 13)))
                    (light-theme-begin 6)   ; Hour to turn on  `light' theme
                    (light-theme-end   17)  ; Hour to turn off `light' theme
                    (light-theme-hours (number-sequence
                                        light-theme-begin light-theme-end))
                    (now (if (member hour light-theme-hours) light dark)))
               (unless (equal now doom-theme)
                 (setq doom-theme now) (doom-init-theme-h)))) light dark)
        ;; Specific color mode
        (setq doom-theme ,my-doom-color) (doom-init-theme-h)))))

(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;
;;; keybinds

(map! :n  "g+"    #'evil-numbers/inc-at-pt
      :v  "g+"    #'evil-numbers/inc-at-pt-incremental
      :nv "g="    #'er/expand-region
      :gi "C-="   #'er/expand-region
      :n  "C-0"   #'doom/reset-font-size
      :n  "C-+"   #'text-scale-increase
      :n  "M-C-+" #'doom/increase-font-size
      :n  "C-SPC" #'just-one-space
      ;; evil Omni-completion, Bind dedicated completion commands
      (:when (and (featurep! :editor evil)
                  (featurep! :completion corfu))
       :i "C-@"   (cmds! (not (minibufferp)) #'completion-at-point)
       :i "C-SPC" (cmds! (not (minibufferp)) #'completion-at-point)
       :prefix "C-x"
       :i "C-p"   #'completion-at-point  ; capf
       :i "C-l"   #'cape-line
       :i "C-k"   #'+cape/dict-or-keywords
       :i "C-a"   #'cape-abbrev
       :i "s"     #'cape-ispell
       (:unless (featurep! :completion company)
        :i "C-s"  #'+cape/yasnippet)
       :i "C-d"   #'cape-dabbrev
       :i "d"     #'dabbrev-completion
       :i "C-f"   #'cape-file
       :i "C-'"   #'cape-symbol
       :i "C-]"   #'complete-tag         ; etags
       :i "C-\\"  #'cape-tex
       :i "&"     #'cape-sgml
       :i "C-r"   #'cape-rfc1345)
      (:when (featurep! :ui doom-dashboard)
       (:map doom-leader-open-map
        "0"       #'+doom-dashboard/open)
       :map +doom-dashboard-mode-map
       :ne "h"    #'+treemacs/toggle
       :ne "l"    #'push-button
       :ne "u"    #'doom/quickload-session
       :ne "a"    #'org-agenda
       :ne "f"    #'find-file
       :ne "e"    #'eww
       :ne "r"    #'consult-recent-file
       :ne "p"    #'projectile-switch-project
       :ne "P"    #'doom/open-private-config
       :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
       :ne "." (defun find-dotfile () (interactive) (doom-project-find-file "~/.config"))
       :ne "b"    #'consult-buffer
       :ne "q"    #'save-buffers-kill-terminal
       :ne "v"    #'+vterm/here
       :ne "t"    #'telega
       :ne "T"    #'=twitter
       :ne "m"    #'mu4e
       :ne "n"    #'+default/find-in-notes
       :ne "d"    #'+workspace/close-window-or-workspace
       :ne "x"    #'org-capture)
      (:when (featurep! :ui hydra)
       :desc "Interactive menu" "<menu>" #'+hydra/window-nav/body
       :leader     :desc "zoom"    "z"   #'+hydra/text-zoom/body
       :when (featurep! :completion vertico)
       [remap +hydra/window-nav/idomenu] #'consult-imenu)
      (:when (featurep! :config default)
       :map  help-map
       "rk"       #'keychain-refresh-environment
       "di"  (cmd! (find-file (expand-file-name "init.org"     doom-private-dir)))
       "do"  (cmd! (find-file (expand-file-name "config.org"   doom-private-dir)))
       "dpo" (cmd! (find-file (expand-file-name "packages.org" doom-private-dir)))
       :leader :prefix-map ("f" . "file") :desc "Find dotfile" "." #'find-dotfile)
      (:when (featurep! :term vterm)
       :map vterm-mode-map
       :i "C-j"   #'vterm--self-insert
       "C-c C-x"  #'vterm--self-insert)
      :desc "Load doom-theme on the fly" "<f5>" (cmd! (doom-init-theme-h))
      (:prefix "C-x"
       :when (featurep! :ui popup)
       :desc "Open this buffer in a popup" "j" #'+popup/buffer)
      :map mode-specific-map            ; C-c
      (:when (featurep! :tools eval)
       :desc "Evaluate line/region"        "e" #'+eval/line-or-region
       :desc "Evaluate & replace region"   "E" #'+eval/region-and-replace)
      (:when (featurep! :tools fzf)
       :desc "Fuzzy find file in project"  "t" #'fzf-projectile))

;;
;;; Time & language

(display-time-mode 1)                   ; Enable time in the mode-line

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)   ; Swap M-/ and C-M-/
         ("C-M-/" . dabbrev-expand)))

;;
;;; Accessibility

(setq-default x-stretch-cursor t)       ; Stretch cursor to the glyph width

;;
;;; Security

(setq password-cache-expiry nil)        ; I can trust my computers … can't I?

;;
;;; Modules

;;; :completion company +tabnine
(when (featurep! :completion company +tabnine)
  (add-to-list 'company-backends #'company-tabnine)
  (after! company
    (setq +lsp-company-backends
          '(company-tabnine :separate company-capf company-yasnippet))
    (setq company-show-numbers t)
    (setq company-idle-delay 0)))

;;; :completion vertico
(when (featurep! :completion vertico +childframe)
  (require 'vertico-posframe)
  (vertico-posframe-mode 1)
  (setq vertico-posframe-border-width 10
        vertico-posframe-parameters
        '((min-width . 90)
          (left-fringe  . 8)
          (right-fringe . 8))))

;;; ui: deft
(setq deft-directory "~/notes")

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t
      ;; By default while in insert all changes are one big blob. Be more granular
      evil-want-fine-undo t)

(add-hook! 'evil-org-mode-hook
  (setq-local evil-surround-pairs-alist
              '((40 "(" . ")")
                (91 "[" . "]")
                (123 "{" . "}")
                (41 "(" . ")")
                (93 "[" . "]")
                (125 "{" . "}")
                (35 "#{" . "}")
                (98 "(" . ")")
                (66 "{" . "}")
                (62 "<" . ">")
                (116 . evil-surround-read-tag)
                (60 . evil-surround-read-tag)
                (102 . evil-surround-function))))

;;; :editor format
(setq-hook! '(js-mode
              js2-mode
              rjsx-mode
              typescript-mode
              typescript-tsx-mode) +format-with-lsp nil)

(use-package arrayify :load-path "lisp") ; ~/.doom.d/lisp/arrayify.el

;;; :tools magit
(setq magit-inhibit-save-previous-winconf t ; Don't restore wconf after quit magit
      forge-database-connector (when EMACS29+ 'sqlite-builtin)) ; buitin support

;;; :lang clojure
(when (featurep! :lang clojure)
  (add-hook 'clojure-mode-hook #'paredit-mode))

;;; :lang web
(use-package! lsp-tailwindcss
  :when (and (featurep! :tools lsp) (featurep! :lang web +tailwind))
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode
                                                typescript-mode typescript-tsx-mode)))

(add-to-list 'lsp-language-id-configuration '(".*\\.liquid" . "html"))

;;; :lang org
(setq org-clock-sound "/mnt/c/Windows/Media/Alarm06.wav"
      org-support-shift-select t
      ;; use g{h,j,k} to traverse headings and TAB to toggle their visibility,
      ;; and leave C-left/C-right to .
      org-tree-slide-skip-outline-level 2
      org-startup-with-inline-images t)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c)))
           string-to-transform)))

(require 'org-protocol)
(setq org-capture-templates
      (append
       org-capture-templates
       `(("P" "Protocol" entry
          (file+headline +org-capture-notes-file "Inbox")
          "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
         ("L" "Protocol Link" entry
          (file+headline +org-capture-notes-file "Inbox")
          "* %? [[%:link][%(transform-square-brackets-to-round-ones
                          \"%:description\")]] \nCaptured On: %U")
         ("l" "Web site" entry
          (file+headline "webnotes.org" "Inbox")
          "* %a\nCaptured On: %U\nWebsite: %l\n\n%i\n%?")
         ("m" "meetup" entry (file "~/org/caldav.org")
          "* %?%:description \n%i\n%l")
         ("w" "Web site" entry
          (file+olp "~/org/inbox.org" "Web")
          "* %c :website:\n%U %?%:initial"))))

(setq  org-roam-capture-ref-templates
       '(("l" "Web site" plain (function org-roam-capture--get-point)
          "${body}\n%?"
          :file-name "%<%Y%m%d>-${slug}"
          :head "#+title: ${title}\n#+CREATED: %U\n#+roam_key: ${ref}\n\n"
          :unnarrowed t)))

;;; :email mu4e
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        mu4e-context-policy         'ask-if-none
        mu4e-compose-context-policy 'always-ask
        mu4e-maildir-shortcuts '(("/thaenalpha@gmail.com/Job Applying".?j)))
  ;; Each path is relative to the path of the maildir you passed to mu
  (set-email-account!
   "boliden@gmail.com"
   '((mu4e-sent-folder       . "/boliden@gmail.com/[Gmail]/Sent Mail")
     (mu4e-drafts-folder     . "/boliden@gmail.com/[Gmail]/Drafts")
     (mu4e-spam-folder       . "/boliden@gmail.com/[Gmail]/Spam")
     (mu4e-trash-folder      . "/boliden@gmail.com/[Gmail]/Trash")
     (mu4e-refile-folder     . "/boliden@gmail.com/[Gmail]/All Mail")
     (smtpmail-smtp-user     . "boliden@gmail.com")
     (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
   t)
  (set-email-account!
   "thaenalpha@gmail.com"
   '((mu4e-sent-folder       . "/thaenalpha@gmail.com/[Gmail]/Sent Mail")
     (mu4e-drafts-folder     . "/thaenalpha@gmail.com/[Gmail]/Drafts")
     (mu4e-spam-folder       . "/thaenalpha@gmail.com/[Gmail]/Spam")
     (mu4e-trash-folder      . "/thaenalpha@gmail.com/[Gmail]/Trash")
     (mu4e-refile-folder     . "/thaenalpha@gmail.com/[Gmail]/All Mail")
     (smtpmail-smtp-user     . "thaenalpha@gmail.com")
     (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
   t)
  (set-email-account!
   "bolidenx@hotmail.com"
   '((mu4e-sent-folder       . "/bolidenx@hotmail.com/Sent")
     (mu4e-drafts-folder     . "/bolidenx@hotmail.com/Drafts")
     (mu4e-spam-folder       . "/bolidenx@hotmail.com/Junk")
     (mu4e-trash-folder      . "/bolidenx@hotmail.com/Deleted")
     (mu4e-refile-folder     . "/bolidenx@hotmail.com/Archive")
     (smtpmail-smtp-user     . "bolidenx@hotmail.com")
     (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
   t)
  (set-email-account!
   "nopanun@live.com"
   '((mu4e-sent-folder       . "/nopanun@live.com/Sent")
     (mu4e-drafts-folder     . "/nopanun@live.com/Drafts")
     (mu4e-spam-folder       . "/nopanun@live.com/Junk")
     (mu4e-trash-folder      . "/nopanun@live.com/Deleted")
     (mu4e-refile-folder     . "/nopanun@live.com/Archive")
     (smtpmail-smtp-user     . "nopanun@live.com")
     (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
   t)
  (set-email-account!
   "tannarin26@yahoo.com"
   '((mu4e-sent-folder       . "/tannarin26@yahoo.com/Sent")
     (mu4e-drafts-folder     . "/tannarin26@yahoo.com/Draft")
     (mu4e-spam-folder       . "/tannarin26@yahoo.com/Bulk Mail")
     (mu4e-trash-folder      . "/tannarin26@yahoo.com/Trash")
     (mu4e-refile-folder     . "/tannarin26@yahoo.com/Archive")
     (smtpmail-smtp-user     . "tannarin26@yahoo.com")
     (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
   t)
  (mapc
   (lambda (bookmark) (add-to-list 'mu4e-bookmarks bookmark))
   `(     ; create bookmarks to show merged views of folders across accounts:
     (,(concat
        "m:/boliden@gmail.com/INBOX or m:/bolidenx@hotmail.com/Inbox or "
        "m:/nopanun@live.com/Inbox or m:/tannarin26@yahoo.com/Inbox or "
        "m:/thaenalpha@gmail.com/INBOX or m:/nopanun@live.com/IT Demands")
      "All Inboxes" ?i)
     (,(concat
        "m:/boliden@gmail.com/[Gmail]/Sent Mail or m:/bolidenx@hotmail.com/Sent "
        "m:/thaenalpha@gmail.com/[Gmail]/Sent Mail or m:/nopanun@live.com/Sent or"
        " m:/tannarin26@yahoo.com/Sent") "All Sent" ?s)
     (,(concat
        "m:/boliden@gmail.com/[Gmail]/Drafts or m:/bolidenx@hotmail.com/Drafts "
        "m:/thaenalpha@gmail.com/[Gmail]/Drafts or m:/nopanun@live.com/Drafts or "
        "m:/tannarin26@yahoo.com/Draft") "All Drafts" ?d)
     (,(concat
        "m:/boliden@gmail.com/[Gmail]/All Mail or m:/bolidenx@hotmail.com/Archive"
        " m:/thaenalpha@gmail.com/[Gmail]/All Mail or m:/nopanun@live.com/Archive"
        " or m:/tannarin26@yahoo.com/Archive") "All Archives" ?a)
     (,(concat
        "m:/boliden@gmail.com/[Gmail]/Spam or m:/bolidenx@hotmail.com/Junk or "
        "m:/thaenalpha@gmail.com/[Gmail]/Spam or m:/nopanun@live.com/Junk or "
        "m:/tannarin26@yahoo.com/Bulk Mail") "All Spams" ?p)
     (,(concat
        "m:/boliden@gmail.com/[Gmail]/Trash or m:/bolidenx@hotmail.com/Deleted or"
        " m:/thaenalpha@gmail.com/[Gmail]/Trash or m:/nopanun@live.com/Deleted or"
        " m:/tannarin26@yahoo.com/Trash") "All Trashes" ?t))))

;;; :term vterm
(add-hook 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

;;
;;; Local Variables
(put 'flycheck-textlint-executable 'safe-local-variable #'stringp)
(put 'quickrun-option-command      'safe-local-variable #'stringp)
