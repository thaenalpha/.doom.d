;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(put 'projectile-project-name 'safe-local-variable #'stringp)

;; For e.g. GPG configuration, email clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")

(setq org-directory "~/org/")
(setq org-clock-sound "/mnt/c/Windows/Media/Alarm06.wav")
(setq doom-theme 'alabaster)
(setq doom-font (font-spec :family "FiraCode" :size 17))
(defun synchronize-theme ()
    (setq hour
        (string-to-number
            (substring (current-time-string) 11 13)))
    (if (member hour (number-sequence 6 17))
        (setq now 'alabaster)
        (setq now 'quartz))
    (if (equal now doom-theme)
        nil
      (progn
        (setq doom-theme now)
        (doom/reload-theme))))

(run-with-timer 0 3600 'synchronize-theme)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; turn on paredit-mode (minor) after Clojure-mode was loaded (major)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook! 'clojure-mode-hook 'turn-on-paredit)

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

; a `fill-column' indicator
(add-hook 'doom-first-buffer-hook #'global-display-fill-column-indicator-mode)

(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))
