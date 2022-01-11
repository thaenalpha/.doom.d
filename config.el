;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; For e.g. GPG configuration, email clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")

;;
;;; System

(setq truncate-string-ellipsis "…")     ; Unicode ellispis are nicer than "...",
                                        ; and also save /precious/ space
(global-subword-mode 1)                 ; Iterate through CamelCase words

(unless (equal "Battery status not available" (battery)) ; on laptops…
  (display-battery-mode 1)) ; it's nice to know how much power you have

(setq-default delete-by-moving-to-trash t) ; Delete files to trash

(setq byte-compile-warnings '(not cl-functions))

(use-package keychain-environment
  :defer t
  :init (keychain-refresh-environment)
  :config (map! :map help-map
                "rk" #'keychain-refresh-environment))

(setq company-idle-delay nil)

(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil))

;;
;;; UI

(setq display-line-numbers-type  nil
      doom-font                  (font-spec :family "JetBrains Mono"
                                            :size 12 :weight 'light)
      doom-variable-pitch-font   (font-spec :family "Noto Serif"     :size 13)
      doom-big-font              (font-spec :family "JetBrains Mono" :size 15)
      doom-unicode-font          (font-spec :family "FiraGO" :weight 'Book)
      doom-serif-font            doom-variable-pitch-font
      window-combination-resize  t ; take new window space from all other windows
      treemacs-width             25
      doom-themes-treemacs-theme 'doom-colors)

(let ((my-doom-color 'auto))
  (if (eq my-doom-color 'auto)
      (run-with-timer
       0 3600                             ; check for every hour
       (defun synchronize-theme ()
         "Sets the theme according to the hour in the current time.
If the hour is (both inclusive) in `light-theme-hours' then
`light' theme is loaded, otherwise `dark' theme is loaded."
         (interactive)
         (let* ((hour (string-to-number (substring (current-time-string) 11 13)))
                (dark  'doom-vibrant)
                (light 'doom-one-light)
                (light-theme-begin 6)   ; Hour to turn on  `light' theme
                (light-theme-end   17)  ; Hour to turn off `light' theme
                (light-theme-hours (number-sequence
                                    light-theme-begin light-theme-end))
                (now (if (member hour light-theme-hours) light dark)))
           (unless (equal now doom-theme)  ; if doom-theme isn't equal to now
             (doom--load-theme-a #'load-theme now t))))) ; set to now and load
    (let ((f (lambda (x) (doom--load-theme-a #'load-theme x t)))
          (default 'doom-one)
          (dark    'doom-vibrant)
          (light   'doom-one-light)
          (custom  'doom-dracula))
      (funcall f (eval my-doom-color)))))

(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;
;;; keybinds

(map! "<f5>"      #'synchronize-theme
      :n  "g+"    #'evil-numbers/inc-at-pt
      :v  "g+"    #'evil-numbers/inc-at-pt-incremental
      :nv "g="    #'er/expand-region
      :gi "C-="   #'er/expand-region
      :n  "C-0"   #'doom/reset-font-size
      :n  "C-+"   #'text-scale-increase
      :n  "M-C-+" #'doom/increase-font-size
      :n  "C-SPC" #'just-one-space
      (:when (featurep! :ui doom-dashboard)
       (:map doom-leader-open-map
        "0" #'+doom-dashboard/open)
       :map +doom-dashboard-mode-map
       :ne "l" #'push-button
       :ne "u" #'doom/quickload-session
       :ne "a" #'org-agenda
       :ne "f" #'find-file
       :ne "r" #'consult-recent-file
       :ne "p" #'projectile-switch-project
       :ne "P" #'doom/open-private-config
       :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
       :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
       :ne "b" #'+vertico/switch-workspace-buffer
       :ne "B" #'consult-buffer
       :ne "q" #'save-buffers-kill-terminal
       :ne "h" #'doom/help
       :ne "v" #'+vterm/here
       :ne "t" #'telega
       :ne "T" #'=twitter
       :ne "m" #'mu4e
       :ne "n" #'+default/find-in-notes
       :ne "d" #'+workspace/close-window-or-workspace
       :ne "x" #'org-capture)
      (:prefix "C-x"
       :when (featurep! :ui popup)
       :desc "Open this buffer in a popup" "j" #'+popup/buffer)
      :map mode-specific-map            ; C-c
      (:when (featurep! :tools eval)
       :desc "Evaluate line/region"      "e"   #'+eval/line-or-region
       :desc "Evaluate & replace region" "E"   #'+eval/region-and-replace)
      (:when (featurep! :tools fzf)
       :desc "Fuzzy find file in project" "t"  #'fzf-projectile))

(after! vterm (evil-collection-vterm-toggle-send-escape)
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-j") 'vterm--self-insert
    (kbd "C-x") 'vterm--self-insert))

window-combination-resize  t ; take new window space from all other windows

;;
;;; Time & language

(display-time-mode 1)                   ; Enable time in the mode-line

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)   ; Swap M-/ and C-M-/
         ("C-M-/" . dabbrev-expand)))

;;
;;; Accessibility

(setq scroll-margin 2)                  ; It's nice to maintain a little margin
;;scroll-preserve-screen-position 'always ; Don't have `point' jump around

(setq-default x-stretch-cursor t)       ; Stretch cursor to the glyph width

;;
;;; Security

(setq password-cache-expiry nil)        ; I can trust my computers … can't I?

;;
;;; Modules

;;; :completion corfu
;; evil Omni-completion, Bind dedicated completion commands
(map! :when (and (featurep! :editor evil)
                 (featurep! :completion corfu))
      :i "C-@"   (cmds! (not (minibufferp)) #'completion-at-point)
      :i "C-SPC" (cmds! (not (minibufferp)) #'completion-at-point)
      :prefix "C-x"
      :i "C-p"  #'completion-at-point  ; capf
      :i "C-l"  #'cape-line
      :i "C-k"  #'+cape/dict-or-keywords
      :i "C-a"  #'cape-abbrev
      :i "s"    #'cape-ispell
      (:unless (featurep! :completion company)
       :i "C-s" #'+cape/yasnippet)
      :i "C-d"  #'cape-dabbrev
      :i "d"    #'dabbrev-completion
      :i "C-f"  #'cape-file
      :i "C-'"  #'cape-symbol
      :i "C-]"  #'complete-tag         ; etags
      :i "C-\\" #'cape-tex
      :i "&"    #'cape-sgml
      :i "C-r"  #'cape-rfc1345)

;;; :completion company +tabnine
(when (featurep! :completion company +tabnine)
  (add-to-list 'company-backends #'company-tabnine)
  (after! company
   (setq +lsp-company-backends
            '(company-tabnine :separate company-capf company-yasnippet))
   (setq company-show-numbers t)
   (setq company-idle-delay 0)))

;; deft
(setq deft-directory "~/notes")

(when (featurep! :ui hydra)
  (when (featurep! :completion vertico)
    (define-key!
      [remap +hydra/window-nav/idomenu] #'consult-imenu))
  (map! :desc "Interactive menu"
        "<menu>" #'+hydra/window-nav/body
        :leader :desc "zoom"
        "z"      #'+hydra/text-zoom/body))

;;; :editor evil
(setq evil-want-fine-undo t)            ; By default while in insert all changes
                                        ; are one big blob. Be more granular

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

(setq-hook! '(js-mode
              js2-mode
              rjsx-mode
              typescript-mode
              typescript-tsx-mode) +format-with-lsp nil)

(use-package arrayify :load-path "lisp") ; ~/.doom.d/lisp/arrayify.el

(require 'button-lock)
(global-fixmee-mode 1)

(when (featurep! :lang clojure)
   (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package! lsp-tailwindcss
  :init
  (setq! lsp-tailwindcss-add-on-mode t)
  :custom
  (lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode typescript-tsx-mode)))

(add-to-list 'lsp-language-id-configuration '(".*\\.liquid" . "html"))

(setq org-clock-sound "/mnt/c/Windows/Media/Alarm06.wav"
      org-support-shift-select t)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c)))
           string-to-transform)))

(require 'org-protocol)
(setq org-capture-templates
  (append org-capture-templates
    `(("P" "Protocol" entry
       (file+headline +org-capture-notes-file "Inbox")
       "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
      ("L" "Protocol Link" entry
       (file+headline +org-capture-notes-file "Inbox")
       "* %? [[%:link][%(transform-square-brackets-to-round-ones
                        \"%:description\")]] \nCaptured On: %U")
      ("l" "Web site" entry
       (file+headline (lambda () (concat org-directory "/webnotes.org")) "Inbox")
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

;; Each path is relative to the path of the maildir you passed to mu
(set-email-account! "boliden@gmail.com"
  '((mu4e-sent-folder       . "/boliden@gmail.com/[Gmail]/Sent Mail")
    (mu4e-drafts-folder     . "/boliden@gmail.com/[Gmail]/Drafts")
    (mu4e-spam-folder       . "/boliden@gmail.com/[Gmail]/Spam")
    (mu4e-trash-folder      . "/boliden@gmail.com/[Gmail]/Trash")
    (mu4e-refile-folder     . "/boliden@gmail.com/[Gmail]/All Mail")
    (smtpmail-smtp-user     . "boliden@gmail.com")
    (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
  t)
(set-email-account! "thaenalpha@gmail.com"
  '((mu4e-sent-folder       . "/thaenalpha@gmail.com/[Gmail]/Sent Mail")
    (mu4e-drafts-folder     . "/thaenalpha@gmail.com/[Gmail]/Drafts")
    (mu4e-spam-folder       . "/thaenalpha@gmail.com/[Gmail]/Spam")
    (mu4e-trash-folder      . "/thaenalpha@gmail.com/[Gmail]/Trash")
    (mu4e-refile-folder     . "/thaenalpha@gmail.com/[Gmail]/All Mail")
    (smtpmail-smtp-user     . "thaenalpha@gmail.com")
    (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
  t)
(set-email-account! "bolidenx@hotmail.com"
  '((mu4e-sent-folder       . "/bolidenx@hotmail.com/Sent")
    (mu4e-drafts-folder     . "/bolidenx@hotmail.com/Drafts")
    (mu4e-spam-folder       . "/bolidenx@hotmail.com/Junk")
    (mu4e-trash-folder      . "/bolidenx@hotmail.com/Deleted")
    (mu4e-refile-folder     . "/bolidenx@hotmail.com/Archive")
    (smtpmail-smtp-user     . "bolidenx@hotmail.com")
    (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
  t)
(set-email-account! "nopanun@live.com"
  '((mu4e-sent-folder       . "/nopanun@live.com/Sent")
    (mu4e-drafts-folder     . "/nopanun@live.com/Drafts")
    (mu4e-spam-folder       . "/nopanun@live.com/Junk")
    (mu4e-trash-folder      . "/nopanun@live.com/Deleted")
    (mu4e-refile-folder     . "/nopanun@live.com/Archive")
    (smtpmail-smtp-user     . "nopanun@live.com")
    (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
  t)
(set-email-account! "tannarin26@yahoo.com"
  '((mu4e-sent-folder       . "/tannarin26@yahoo.com/Sent")
    (mu4e-drafts-folder     . "/tannarin26@yahoo.com/Draft")
    (mu4e-spam-folder       . "/tannarin26@yahoo.com/Bulk Mail")
    (mu4e-trash-folder      . "/tannarin26@yahoo.com/Trash")
    (mu4e-refile-folder     . "/tannarin26@yahoo.com/Archive")
    (smtpmail-smtp-user     . "tannarin26@yahoo.com")
    (mu4e-compose-signature . "---\nNopanun Laochunhanun"))
  t)

(setq mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'always-ask)

(after! mu4e
 (setq sendmail-program (executable-find "msmtp")
       send-mail-function #'smtpmail-send-it
       message-sendmail-f-is-evil t
       message-sendmail-extra-arguments '("--read-envelope-from")
       message-send-mail-function #'message-send-mail-with-sendmail
       mu4e-maildir-shortcuts '(("/thaenalpha@gmail.com/Job Applying".?j)))

 (defun add-mu4e-bookmark (bookmark)
   (add-to-list 'mu4e-bookmarks bookmark))

 (mapc 'add-mu4e-bookmark
   '(("m:/boliden@gmail.com/INBOX or m:/bolidenx@hotmail.com/Inbox or m:/nopanun@live.com/Inbox or m:/tannarin26@yahoo.com/Inbox or m:/thaenalpha@gmail.com/INBOX or m:/nopanun@live.com/IT Demands" "All Inboxes" ?i)
     ("m:/boliden@gmail.com/[Gmail]/Sent Mail or m:/bolidenx@hotmail.com/Sent m:/thaenalpha@gmail.com/[Gmail]/Sent Mail or m:/nopanun@live.com/Sent or m:/tannarin26@yahoo.com/Sent" "All Sent" ?s)
     ("m:/boliden@gmail.com/[Gmail]/Drafts or m:/bolidenx@hotmail.com/Drafts m:/thaenalpha@gmail.com/[Gmail]/Drafts or m:/nopanun@live.com/Drafts or m:/tannarin26@yahoo.com/Draft" "All Drafts" ?d)
     ("m:/boliden@gmail.com/[Gmail]/All Mail or m:/bolidenx@hotmail.com/Archive m:/thaenalpha@gmail.com/[Gmail]/All Mail or m:/nopanun@live.com/Archive or m:/tannarin26@yahoo.com/Archive" "All Archives" ?a)
     ("m:/boliden@gmail.com/[Gmail]/Spam or m:/bolidenx@hotmail.com/Junk or m:/thaenalpha@gmail.com/[Gmail]/Spam or m:/nopanun@live.com/Junk or m:/tannarin26@yahoo.com/Bulk Mail" "All Spams" ?p)
     ("m:/boliden@gmail.com/[Gmail]/Trash or m:/bolidenx@hotmail.com/Deleted or m:/thaenalpha@gmail.com/[Gmail]/Trash or m:/nopanun@live.com/Deleted or m:/tannarin26@yahoo.com/Trash" "All Trashes" ?t))))

(put 'flycheck-textlint-executable 'safe-local-variable #'stringp)
(put 'quickrun-option-command 'safe-local-variable #'stringp)
