;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; For e.g. GPG configuration, email clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")
(when IS-MAC                            ; Ghub's preference.
  (setq auth-sources `("~/.authinfo.gpg" macos-keychain-generic macos-keychain-internet
                       password-store ,(expand-file-name "authinfo.gpg" doom-etc-dir))))

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

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq-default delete-by-moving-to-trash t) ; Delete files to trash

(when (featurep 'keychain-environment)
 (add-hook! magit-mode #'keychain-refresh-environment))

;;
;;; UI

(setq display-line-numbers-type  'visual
      doom-font                  (font-spec :family "JetBrainsMono Nerd Font"
                                            :size 12 :weight 'light)
      doom-variable-pitch-font   (font-spec :family "DejaVu Sans" :size 13)
      vertico-posframe-font      (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-unicode-font          (font-spec :family "FiraGO" :weight 'Book)
      doom-serif-font            doom-variable-pitch-font
      default-frame-alist        (append default-frame-alist
                                         '((height . 50)
                                           (width  . 162)
                                           (inhibit-double-buffering . t)))
      doom-acario-light-brighter-modeline t
      doom-themes-treemacs-theme 'doom-colors
      doom-modeline-height 22
      aj-dark+-blue-modeline t
      treemacs-width             32
      +treemacs-git-mode         'deferred)

(let ((my-doom-color 'auto))
  (eval                           ; theme varies to the value of `my-doom-color'
   `(let ((auto       'auto)
          (default    'doom-one)
          (light ',(nth (random 5)
                        '(alabaster doom-alabaster-bg doom-alabaster
                          doom-acario-light doom-github)))
          (dark ',(nth (random 5)
                       '(aj-dark+ doom-one doom-vibrant doom-ayu-mirage doom-dracula)))
          (custom     'doom-dracula))
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
                    (light-theme-end  17)   ; Hour to turn off `light' theme
                    (light-theme-hours (number-sequence
                                        light-theme-begin light-theme-end))
                    (now (if (member hour light-theme-hours) light dark)))
               (unless (equal now doom-theme)
                 (setq doom-theme now) (doom-init-theme-h)))) light dark)
        ;; Specific color mode
        (setq doom-theme ,my-doom-color) (doom-init-theme-h)))))

;; Roll the mouse wheel to scrolls the display pixel-by-pixel.
(when (fboundp #'pixel-scroll-precision-mode) ; EMACS29+
  (pixel-scroll-precision-mode t))

;;
;;; keybinds

(map! :desc "Load doom-theme on the fly" "<f5>" (cmd! (doom-init-theme-h))
      ;;; C-c
      (:prefix ("C-c" . "mode-specific-map")
       (:when (featurep! :tools eval)
        :desc "Evaluate line/region"        "e" #'+eval/line-or-region
        :desc "Evaluate & replace region"   "E" #'+eval/region-and-replace)
       (:when (featurep! :checkers grammar)
        "g"    #'writegood-mode
        "C-g g" #'writegood-grade-level
        "C-g e" #'writegood-reading-ease))

      ;;
      ;;; evil

      :when (featurep! :editor evil)
      :n  "g+"    #'evil-numbers/inc-at-pt
      :v  "g+"    #'evil-numbers/inc-at-pt-incremental
      :nv "g="    #'er/expand-region
      :gi "C-="   #'er/expand-region
      :n  "C-0"   #'doom/reset-font-size
      :n  "C-+"   #'text-scale-increase
      :n  "M-C-+" #'doom/increase-font-size
      (:when (or IS-WINDOWS
                 (getenv "WSLENV"))
       :n  "C-SPC" #'just-one-space)

      ;;; :ui doom-dashboard
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
       :ne "c"    (cmd! (find-file (expand-file-name
                                    "config.org" doom-private-dir)))
       :ne "."    (defun find-dotfile () (interactive)
                         (doom-project-find-file "~/.config"))
       :ne "b"    #'consult-buffer
       :ne "q"    #'save-buffers-kill-terminal
       :ne "v"    #'+vterm/here
       :ne "t"    #'telega
       :ne "T"    #'=twitter
       :ne "m"    #'mu4e
       :ne "n"    #'+default/find-in-notes
       :ne "d"    #'+workspace/close-window-or-workspace
       :ne "x"    #'org-capture)

      ;;; :ui hydra
      (:when (featurep! :ui hydra)
       :desc "Interactive menu" "<menu>" #'+hydra/window-nav/body
       :when (featurep! :completion vertico)
       [remap +hydra/window-nav/idomenu] #'consult-imenu)

      (:when (featurep! :emacs dired +dirvish)
       :map dired-mode-map
       "zz"      #'dirvish-show-history
       "M-c"     #'dirvish-ui-config
       "M-m"     #'dirvish-toggle-fullscreen
       "C-c C-f" #'fd-dired
       [remap dired-do-copy]       #'dirvish-yank
       [remap dired-do-redisplay]  #'dirvish-roam
       [remap evil-backward-char]  #'dirvish-up-directory
       [remap evil-forward-char]   #'dirvish-find-file
       [remap evil-find-char]      #'dirvish-menu-file-info-cmds
       [remap evil-ex-search-backward] #'dirvish-dispatch
       [remap evil-switch-to-windows-last-buffer] #'dirvish-other-buffer)

      (:when (featurep! :term vterm)
       :map vterm-mode-map
       :i "C-j"   #'vterm--self-insert
       "C-c C-x"  #'vterm--self-insert)

      ;;; :completion corfu
      (:when (featurep! :completion corfu)
       :i "C-@"   (cmds! (not (minibufferp)) #'completion-at-point)
       :i "C-SPC" (cmds! (not (minibufferp)) #'completion-at-point))

      ;;; C-x
      (:prefix "C-x"
       ;; Omni-completion, Bind dedicated completion commands
       (:when (featurep! :completion corfu)
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
       :when (featurep! :ui popup)
       :desc "Open this buffer in a popup" "j" #'+popup/buffer)

      ;;; :config default
      (:when (featurep! :config default)
       (:map help-map
        (:when (featurep 'keychain-environment)
         "rk"   #'keychain-refresh-environment)
        :prefix "d"
        :desc "init.org"        "i"  (cmd! (find-file
              (expand-file-name "init.org" doom-private-dir)))
        :desc "config.org"      "o"  (cmd! (find-file
              (expand-file-name "config.org" doom-private-dir)))
        :desc "packages.org"    "po" (cmd! (find-file
              (expand-file-name "packages.org" doom-private-dir))))

       ;;; <leader>
       (:when (featurep! :config default +bindings)
        (:leader
         ;; <leader> z --- zoom
         (:when (featurep! :ui hydra)
          :desc  "Text zoom menu"   "z"    #'+hydra/text-zoom/body)
         ;; <leader> f --- file
         (:prefix-map ("f" . "file") :desc "Find dotfile" "." #'find-dotfile)
         ;; <leader> g --- git/version control
         (:prefix-map ("g" . "git")
          (:prefix ("l" . "list")
           (:when (featurep! :tools gist)
            :desc "List other user's gists" "u"   #'gist-list-user
            :desc "List your starred gists" "M-s" #'gist-list-starred)))))))



;;
;;; Time & language

(display-time-mode 1)                   ; Enable time in the mode-line

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)   ; Swap M-/ and C-M-/
         ("C-M-/" . dabbrev-expand)))

;;
;;; Accessibility

;; Nice scrolling
(setq scroll-conservatively 100000
      scroll-preserve-screen-position 1) ; Don't have `point' jump around

(setq-default x-stretch-cursor t)       ; Stretch cursor to the glyph width

;;
;;; Security

(setq password-cache-expiry nil)        ; I can trust my computers … can't I?

;;
;;; Modules

;;; :app mastodon
(after! mastodon
 (setq mastodon-instance-url "https://mstdn.io"))

;;; :app reddit
(after! md4rd
  (let ((reddit-auth (lambda (type)
                       (funcall
                        (plist-get (car (auth-source-search :user type))
                                   :secret)))))
    (setq md4rd-subs-active
          '(
            emacs+doomemacs+orgmode lisp+Common_Lisp+prolog+clojure javascript
            linux firefox ProgrammerHumor programming+learnprogramming webdev
            guix bashonubuntuonwindows hackernews graphql cscareerquestions)
          md4rd--oauth-access-token (funcall
                                     reddit-auth "me^access-token")
          md4rd--oauth-refresh-token (funcall
                                      reddit-auth "me^refresh-token")))
  (run-with-timer 0 3540 #'md4rd-refresh-login))

(after! langtool
  (unless (or langtool-bin
              langtool-language-tool-jar
              langtool-java-classpath)
    (cond (IS-MAC
           (cond
            ;; is user using home brew?
            ((file-directory-p "/opt/homebrew/Cellar/languagetool")
             (setq langtool-language-tool-jar
                   (locate-file "libexec/languagetool-commandline.jar"
                                (doom-files-in "/opt/homebrew/Cellar/languagetool"
                                               :type 'dirs
                                               :depth 2)))))))))

;;; :completion company +tabnine
(when (featurep! :completion company +tabnine)
  (add-to-list 'company-backends #'company-tabnine)
  (after! company
    (setq company-idle-delay nil
          +lsp-company-backends
          '(company-tabnine :separate company-capf company-yasnippet)
          company-show-numbers t
          company-idle-delay 0)))

;;; :completion vertico +childframe
(when (featurep! :completion vertico +childframe)
  (setq vertico-posframe-border-width 10
        vertico-posframe-parameters '((left-fringe  . 8) (right-fringe . 8)
                                      (min-width . 90))
        vertico-posframe-poshandler #'posframe-poshandler-frame-center))

;;; :config literate
(defvar +literate-tangle--proc nil)
(defvar +literate-tangle--proc-start-time nil)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (unless (getenv "__NOTANGLE")
    (let ((default-directory doom-private-dir))
      (when +literate-tangle--proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-tangle--proc #'ignore)
        (kill-process +literate-tangle--proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-tangle--proc-start-time (float-time)
            +literate-tangle--proc
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-confirm-babel-evaluate nil \
      org-inhibit-startup t \
      org-mode-hook nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      vc-handled-backends nil \
      org-startup-folded nil \
      org-startup-indented nil) \
(org-babel-tangle-file \"%s\" \"%s\"))"
                                   +literate-config-file
                                   (expand-file-name (concat doom-module-config-file ".el")))))
      (set-process-sentinel +literate-tangle--proc #'+literate-tangle--sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-tangle--sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (pop-to-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))))

(defun +literate-tangle-check-finished ()
  (when (and (process-live-p +literate-tangle--proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))
(add-hook! 'kill-emacs-hook #'+literate-tangle-check-finished)

;;; ui: deft
(setq deft-directory "~/notes")

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape - @hlissner
(advice-add #'doom-modeline-segment--modals :override #'ignore)

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

;;; :emacs dired +dirvish
(when (featurep! :emacs dired +dirvish)
  ;; (add-hook! dirvish-mode (defun dirvish--normalize-keymaps ()
  ;;                          (when (boundp 'evil-mode)
  ;;                            ;; turn off evilified buffers for evilify usage
  ;;                            (evil-make-overriding-map dirvish-mode-map 'motion)
  ;;                            (evil-normalize-keymaps))))
  (after! dired
    (dirvish-override-dired-mode)
    ;; Enable file preview when narrowing files in minibuffer.
    (dirvish-peek-mode)))

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

;;; :lang clojure
(when (featurep! :lang clojure)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (require 'clj-deps-new))

;;; :lang org
(setq org-support-shift-select t
      ;; use g{h,j,k} to traverse headings and TAB to toggle their visibility,
      ;; and leave C-left/C-right to .
      org-tree-slide-skip-outline-level 2)
(add-hook 'org-mode-hook #'org-modern-mode)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c)))
           string-to-transform)))

(after! org
  (setq org-startup-with-inline-images t
        org-capture-templates
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
            "* %c :website:\n%U %?%:initial")))))

(after! org-roam
  (setq org-roam-capture-ref-templates
        '(("l" "Web site" plain (function org-roam-capture--get-point)
           "${body}\n%?"
           :file-name "%<%Y%m%d>-${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+roam_key: ${ref}\n\n"
           :unnarrowed t))))

(when (featurep 'sql-indent-autoloads) (add-hook! sql-mode #'sqlind-minor-mode))
;; MySQL
(require 'emacsql-mysql)

;;; :lang web
(use-package! lsp-tailwindcss
  :when (and (featurep! :tools lsp) (featurep! :lang web +tailwind))
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode
                                                typescript-mode typescript-tsx-mode)))

(add-to-list 'lsp-language-id-configuration '(".*\\.liquid" . "html"))

;;; :term vterm
(add-hook 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

(use-package arrayify :load-path "lisp") ; ~/.doom.d/lisp/arrayify.el

;;; :tools gist
(add-hook! gist-list-mode #'turn-off-evil-snipe-mode)

;;; :tools LSP -- https://git.sr.ht/~gagbo/doom-config
(unless (featurep! :checkers syntax)
  (setq lsp-diagnostics-provider :flymake))

(after! lsp-mode
  (setq
   lsp-auto-guess-root t
   lsp-enable-semantic-tokens-enable nil
   lsp-progress-via-spinner nil
   lsp-idle-delay 0.47
   lsp-completion-enable-additional-text-edit nil
   lsp-signature-render-documentation t
   lsp-signature-auto-activate t
   lsp-signature-doc-lines 5
   lsp-eldoc-enable-hover t
   lsp-headerline-breadcrumb-enable nil
   lsp-print-performance nil
   lsp-enable-indentation t
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-links nil
   lsp-log-io nil))

(setq +lsp-company-backends '(company-capf :with company-yasnippet))

(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-doc-delay 0.73
        lsp-ui-doc-max-width 50
        lsp-ui-doc-max-height 10
        lsp-ui-doc-include-signature t
        lsp-ui-doc-header t)

  (add-hook! 'lsp-ui-mode-hook
    (run-hooks (intern (format "%s-lsp-ui-hook" major-mode)))))

;;; :tools magit
(setq magit-inhibit-save-previous-winconf t) ; Don't restore wconf after quit magit
(when EMACS29+ ; sqlite buitin support
  (setq forge-database-connector 'sqlite-builtin))

;;
;;; Local Variables
(put 'flycheck-textlint-executable 'safe-local-variable #'stringp)
(put 'quickrun-option-command      'safe-local-variable #'stringp)
