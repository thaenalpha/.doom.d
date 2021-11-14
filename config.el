;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; For e.g. GPG configuration, email clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")

(setq light 'doom-acario-light)
(setq dark 'doom-ayu-mirage)

(setq doom-theme light)            ; default in Light mode

(defun synchronize-theme ()
  (setq hour                          ; current hour
      (string-to-number
          (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 17)) ; Check if daytime's period
      (setq now light)                     ; true: Light
      (setq now dark))                     ; else: Dark
  (if (equal now doom-theme)          ; if now is Light
      nil                             ; do nothing
    (progn                            ; else
      (setq doom-theme now)           ; set to Dark
      (doom/reload-theme))))          ; and reload

(run-with-timer 0 3600 'synchronize-theme) ; check for every hour

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Noto Serif" :size 13)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 19))

(map! :map +doom-dashboard-mode-map
      :ne "l" #'doom/quickload-session
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
      :ne "m" #'=mu4e
      :ne "n" #'+default/find-in-notes)

(use-package! keychain-environment
  :init (keychain-refresh-environment)
  :config (map! :map help-map
                "rk" #'keychain-refresh-environment))

(put 'projectile-project-name 'safe-local-variable #'stringp)
(put 'flycheck-textlint-executable 'safe-local-variable #'stringp)

;; deft
(setq deft-directory "~/notes")

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

(setq org-clock-sound "/mnt/c/Windows/Media/Alarm06.wav")

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c)))
           string-to-transform)))

(require 'org-protocol)
(setq org-capture-templates
  (append org-capture-templates
    `(("P" "Protocol" entry
       (file+headline ,(concat org-directory "notes.org") "Inbox")
       "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
      ("L" "Protocol Link" entry
       (file+headline ,(concat org-directory "notes.org") "Inbox")
       "* %? [[%:link][%(transform-square-brackets-to-round-ones
                        \"%:description\")]] \nCaptured On: %U")
      ("w" "Web site" entry
       (file+olp "~/org/inbox.org" "Web")
       "* %c :website:\n%U %?%:initial"))))

(when (featurep! :lang clojure)
   (add-hook 'clojure-mode-hook 'paredit-mode))
