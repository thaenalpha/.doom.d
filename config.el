;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; For e.g. GPG configuration, email clients, file templates and snippets.
(setq user-full-name "Nopanun Laochunhanun"
      user-mail-address "nopanun@pm.me")
(when IS-MAC                            ; Ghub's preference.
  (setq auth-sources `("~/.authinfo.gpg" macos-keychain-generic macos-keychain-internet
                       password-store ,(expand-file-name "authinfo.gpg" doom-etc-dir))))

;;
;;; System

(setq shell-command-switch "-ic"
      window-combination-resize t ; take new window space from all other windows
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
      +treemacs-git-mode         'deferred)

(let ((my-doom-color 'auto))
  (eval                           ; theme varies to the value of `my-doom-color'
   `(let ((auto       'auto)
          (default    'doom-one)
          (light ',(nth (random 7)
                        '(alabaster doom-alabaster-bg doom-alabaster doom-acario-light
                                    doom-github almost-mono-white almost-mono-cream)))
          (dark ',(nth (random 8)
                       '(aj-dark+ doom-one doom-vibrant doom-ayu-mirage doom-dracula
                                  ahungry almost-mono-black almost-mono-gray)))
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

(set-mouse-color "red")

;;
;;; keybinds

(map! (:after dabbrev
       "M-/"   #'dabbrev-completion   ; Swap M-/ and C-M-/
       "C-M-/" #'dabbrev-expand)
      :desc "Load doom-theme on the fly"    "<f5>"  (cmd! (doom-init-theme-h))
      :desc "Org-capture bin"               "s-X"   #'+org-capture/open-frame

      ;;; C-c
      (:prefix ("C-c" . "mode-specific-map")
       (:when (featurep! :tools eval)
        :desc "Evaluate line/region"        "e"     #'+eval/line-or-region
        :desc "Evaluate & replace region"   "E"     #'+eval/region-and-replace)
       (:when (featurep! :checkers grammar)
        "g"     #'writegood-mode
        "C-g g" #'writegood-grade-level
        "C-g e" #'writegood-reading-ease))

      (:when IS-MAC
       :desc "Next buffer"                  "s-]"   #'next-buffer
       :desc "Previous buffer"              "s-["   #'previous-buffer)

      ;;
      ;;; evil

      :when (featurep! :editor evil)
      :desc "Next file"                     "M-]"   #'+evil/next-file
      :desc "Previous file"                 "M-["   #'+evil/previous-file
      :n  "g+"    #'evil-numbers/inc-at-pt
      :v  "g+"    #'evil-numbers/inc-at-pt-incremental
      :nv "g="    #'er/expand-region
      :gi "C-="   #'er/expand-region
      :n  "C-0"   #'doom/reset-font-size
      :n  "C-+"   #'text-scale-increase
      :n  "M-C-+" #'doom/increase-font-size
      (:when (or IS-WINDOWS
                 (getenv "WSLENV"))
       :n "C-SPC" #'just-one-space)

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

      ;;; Org extra functionality
      (:map org-mode-map
       "<f12>" #'org-transclusion-add
       ;; "C-c e" #'org-toggle-emphasis
       :localleader
       :desc "Outline" "O" #'org-ol-tree)

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
        :i "C-r"   #'cape-rfc1345))

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

       ;;; :ui
       (:when (featurep! :ui popup)
        :desc "Open this buffer in a popup" "C-x j" #'+popup/buffer)
       (:when (featurep! :ui workspaces)
        (:when IS-MAC
         :desc "Next workspace"             "s-}"   #'+workspace/switch-right
         :desc "Previous workspace"         "s-{"   #'+workspace/switch-left))

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
            :desc "List your starred gists" "M-s" #'gist-list-starred)))
         ;; <leader> n --- notes
         (:prefix-map ("n" . "notes")
          :desc "Org Transclusion Mode" "T" #'org-transclusion-mode)
         ;; <leader> p --- project
         (:prefix-map ("p" . "project")
          :when (featurep! :tools prodigy)
          :desc "services"                 "t" #'prodigy)
         ;; <leader> s --- search
         (:prefix-map ("s" . "search")
          (:prefix-map ("a" . "Search in applications")
           (:when (and IS-MAC (featurep! :tools lookup +docsets))
            :desc "dash at point" "d" #'dash-at-point)))))))



;;
;;; Time & language

(display-time-mode 1)                   ; Enable time in the mode-line

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

(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

(setq +zen-text-scale 0.8)

(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(defvar +zen-org-starhide t
  "The value `org-modern-hide-stars' is set to.")

(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-modern-mode
            'org-modern-star
            'org-modern-hide-stars)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-prose-org-h ()
              "Reformat the current Org buffer appearance for prose."
              (when (eq major-mode 'org-mode)
                (setq display-line-numbers nil
                      visual-fill-column-width 60
                      org-adapt-indentation nil)
                (when (featurep 'org-modern)
                  (setq-local org-modern-star '("🙘" "🙙" "🙚" "🙛")
                              ;; org-modern-star '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                              org-modern-hide-stars +zen-org-starhide)
                  (org-modern-mode -1)
                  (org-modern-mode 1))
                (setq
                 +zen--original-org-indent-mode-p org-indent-mode)
                 (org-indent-mode -1)))
            (add-hook 'writeroom-mode-disable-hook
                      (defun +zen-nonprose-org-h ()
                        "Reverse the effect of `+zen-prose-org'."
                        (when (eq major-mode 'org-mode)
                          (when (bound-and-true-p org-modern-mode)
                            (org-modern-mode -1)
                            (org-modern-mode 1))
                          (when +zen--original-org-indent-mode-p (org-indent-mode 1)))))))

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
(after! org
  (setq org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-support-shift-select t
        ;; use g{h,j,k} to traverse headings and TAB to toggle their visibility
        ;; and leave C-left/C-right to .
        org-tree-slide-skip-outline-level 2)
  (after! org-capture
    (defun org-capture-select-template-prettier (&optional keys)
      "Select a capture template, in a prettier way than default
    Lisp programs can force the template by setting KEYS to a string."
      (let ((org-capture-templates
             (or (org-contextualize-keys
                  (org-capture-upgrade-templates org-capture-templates)
                  org-capture-templates-contexts)
                 '(("t" "Task" entry (file+headline "" "Tasks")
                    "* TODO %?\n  %u\n  %a")))))
        (if keys
            (or (assoc keys org-capture-templates)
                (error "No capture template referred to by \"%s\" keys" keys))
          (org-mks org-capture-templates
                   "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                   "Template key: "
                   `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
    (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
    
    (defun org-mks-pretty (table title &optional prompt specials)
      "Select a member of an alist with multiple keys. Prettified.
    
    TABLE is the alist which should contain entries where the car is a string.
    There should be two types of entries.
    
    1. prefix descriptions like (\"a\" \"Description\")
       This indicates that `a' is a prefix key for multi-letter selection, and
       that there are entries following with keys like \"ab\", \"ax\"…
    
    2. Select-able members must have more than two elements, with the first
       being the string of keys that lead to selecting it, and the second a
       short description string of the item.
    
    The command will then make a temporary buffer listing all entries
    that can be selected with a single key, and all the single key
    prefixes.  When you press the key for a single-letter entry, it is selected.
    When you press a prefix key, the commands (and maybe further prefixes)
    under this key will be shown and offered for selection.
    
    TITLE will be placed over the selection in the temporary buffer,
    PROMPT will be used when prompting for a key.  SPECIALS is an
    alist with (\"key\" \"description\") entries.  When one of these
    is selected, only the bare key is returned."
      (save-window-excursion
        (let ((inhibit-quit t)
              (buffer (org-switch-to-buffer-other-window "*Org Select*"))
              (prompt (or prompt "Select: "))
              case-fold-search
              current)
          (unwind-protect
              (catch 'exit
                (while t
                  (setq-local evil-normal-state-cursor (list nil))
                  (erase-buffer)
                  (insert title "\n\n")
                  (let ((des-keys nil)
                        (allowed-keys '("\C-g"))
                        (tab-alternatives '("\s" "\t" "\r"))
                        (cursor-type nil))
                    ;; Populate allowed keys and descriptions keys
                    ;; available with CURRENT selector.
                    (let ((re (format "\\`%s\\(.\\)\\'"
                                      (if current (regexp-quote current) "")))
                          (prefix (if current (concat current " ") "")))
                      (dolist (entry table)
                        (pcase entry
                          ;; Description.
                          (`(,(and key (pred (string-match re))) ,desc)
                           (let ((k (match-string 1 key)))
                             (push k des-keys)
                             ;; Keys ending in tab, space or RET are equivalent.
                             (if (member k tab-alternatives)
                                 (push "\t" allowed-keys)
                               (push k allowed-keys))
                             (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                          ;; Usable entry.
                          (`(,(and key (pred (string-match re))) ,desc . ,_)
                           (let ((k (match-string 1 key)))
                             (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                             (push k allowed-keys)))
                          (_ nil))))
                    ;; Insert special entries, if any.
                    (when specials
                      (insert "─────────────────────────\n")
                      (pcase-dolist (`(,key ,description) specials)
                        (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                        (push key allowed-keys)))
                    ;; Display UI and let user select an entry or
                    ;; a sub-level prefix.
                    (goto-char (point-min))
                    (unless (pos-visible-in-window-p (point-max))
                      (org-fit-window-to-buffer))
                    (let ((pressed (org--mks-read-key allowed-keys
                                                      prompt
                                                      (not (pos-visible-in-window-p (1- (point-max)))))))
                      (setq current (concat current pressed))
                      (cond
                       ((equal pressed "\C-g") (user-error "Abort"))
                       ;; Selection is a prefix: open a new menu.
                       ((member pressed des-keys))
                       ;; Selection matches an association: return it.
                       ((let ((entry (assoc current table)))
                          (and entry (throw 'exit entry))))
                       ;; Selection matches a special entry: return the
                       ;; selection prefix.
                       ((assoc current specials) (throw 'exit current))
                       (t (error "No entry available")))))))
            (when buffer (kill-buffer buffer))))))
    (advice-add 'org-mks :override #'org-mks-pretty)
  
    (defun +doct-icon-declaration-to-icon (declaration)
      "Convert :icon declaration to icon"
      (let ((name (pop declaration))
            (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
            (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
            (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
        (apply set `(,name :face ,face :v-adjust ,v-adjust))))
  
    (defun +doct-iconify-capture-templates (groups)
      "Add declaration's :icon to each template group in GROUPS."
      (let ((templates (doct-flatten-lists-in groups)))
        (setq doct-templates (mapcar (lambda (template)
                                       (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                   (spec (plist-get (plist-get props :doct) :icon)))
                                         (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                        "\t"
                                                                        (nth 1 template))))
                                       template)
                                     templates))))
  
    (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))
  
    ;; (defvar +org-capture-recipies  "~/Desktop/TEC/Organisation/recipies.org")
  
    (defun set-org-capture-templates ()
      (setq org-capture-templates
            (doct `(("Personal todo" :keys "t"
                     :icon ("checklist" :set "octicon" :color "green")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* TODO %?"
                                "%i %a"))
                    ("Personal note" :keys "n"
                     :icon ("sticky-note-o" :set "faicon" :color "green")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* %?"
                                "%i %a"))
                    ("Email" :keys "e"
                     :icon ("envelope" :set "faicon" :color "blue")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                                "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                                "about %^{topic}"
                                "%U %i %a"))
                    ("Interesting" :keys "i"
                     :icon ("eye" :set "faicon" :color "lcyan")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Interesting"
                     :type entry
                     :template ("* [ ] %{desc}%? :%{i-type}:"
                                "%i %a")
                     :children (("Webpage" :keys "w"
                                 :icon ("globe" :set "faicon" :color "green")
                                 :desc "%(org-cliplink-capture) "
                                 :i-type "read:web")
                                ("Article" :keys "a"
                                 :icon ("file-text" :set "octicon" :color "yellow")
                                 :desc ""
                                 :i-type "read:reaserch")
                                ("\tRecipie" :keys "r"
                                 :icon ("spoon" :set "faicon" :color "dorange")
                                 :file +org-capture-recipies
                                 :headline "Unsorted"
                                 :template "%(org-chef-get-recipe-from-url)")
                                ("Information" :keys "i"
                                 :icon ("info-circle" :set "faicon" :color "blue")
                                 :desc ""
                                 :i-type "read:info")
                                ("Idea" :keys "I"
                                 :icon ("bubble_chart" :set "material" :color "silver")
                                 :desc ""
                                 :i-type "idea")))
                    ("Tasks" :keys "k"
                     :icon ("inbox" :set "octicon" :color "yellow")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Tasks"
                     :type entry
                     :template ("* TODO %? %^G%{extra}"
                                "%i %a")
                     :children (("General Task" :keys "k"
                                 :icon ("inbox" :set "octicon" :color "yellow")
                                 :extra "")
                                ("Task with deadline" :keys "d"
                                 :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                 :extra "\nDEADLINE: %^{Deadline:}t")
                                ("Scheduled Task" :keys "s"
                                 :icon ("calendar" :set "octicon" :color "orange")
                                 :extra "\nSCHEDULED: %^{Start time:}t")))
                    ("Project" :keys "p"
                     :icon ("repo" :set "octicon" :color "silver")
                     :prepend t
                     :type entry
                     :headline "Inbox"
                     :template ("* %{time-or-todo} %?"
                                "%i"
                                "%a")
                     :file ""
                     :custom (:time-or-todo "")
                     :children (("Project-local todo" :keys "t"
                                 :icon ("checklist" :set "octicon" :color "green")
                                 :time-or-todo "TODO"
                                 :file +org-capture-project-todo-file)
                                ("Project-local note" :keys "n"
                                 :icon ("sticky-note" :set "faicon" :color "yellow")
                                 :time-or-todo "%U"
                                 :file +org-capture-project-notes-file)
                                ("Project-local changelog" :keys "c"
                                 :icon ("list" :set "faicon" :color "blue")
                                 :time-or-todo "%U"
                                 :heading "Unreleased"
                                 :file +org-capture-project-changelog-file)))
                    ("\tCentralised project templates"
                     :keys "o"
                     :type entry
                     :prepend t
                     :template ("* %{time-or-todo} %?"
                                "%i"
                                "%a")
                     :children (("Project todo"
                                 :keys "t"
                                 :prepend nil
                                 :time-or-todo "TODO"
                                 :heading "Tasks"
                                 :file +org-capture-central-project-todo-file)
                                ("Project note"
                                 :keys "n"
                                 :time-or-todo "%U"
                                 :heading "Notes"
                                 :file +org-capture-central-project-notes-file)
                                ("Project changelog"
                                 :keys "c"
                                 :time-or-todo "%U"
                                 :heading "Unreleased"
                                 :file +org-capture-central-project-changelog-file)))))))
  
    (set-org-capture-templates)
    (unless (display-graphic-p)
      (add-hook 'server-after-make-frame-hook
                (defun org-capture-reinitialise-hook ()
                  (when (display-graphic-p)
                    (set-org-capture-templates)
                    (remove-hook 'server-after-make-frame-hook
                                 #'org-capture-reinitialise-hook))))))
  (setf (alist-get 'height +org-capture-frame-parameters) 15)
  ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
  (setq +org-capture-fn
        (lambda ()
          (interactive)
          (set-window-parameter nil 'mode-line-format 'none)
          (org-capture))))

(add-hook 'org-mode-hook #'org-modern-mode)

(add-hook! org-mode #'org-appear-mode)
(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(after! org-ol-tree
  (defadvice! org-ol-tree-system--graphical-frame-p--pgtk ()
    :override #'org-ol-tree-system--graphical-frame-p
    (memq window-system '(pgtk x w32 ns))))

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c)))
           string-to-transform)))

(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("r" "thought" plain
           ,(format "#+title: ${title}\n%%[%s/template/thought.org]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "topic" plain
           ,(format "#+title: ${title}\n%%[%s/template/topic.org]" org-roam-directory)
           :target (file "topic/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "contact" plain
           ,(format "#+title: ${title}\n%%[%s/template/contact.org]" org-roam-directory)
           :target (file "contact/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           ,(format "#+title: ${title}\n%%[%s/template/project.org]" org-roam-directory)
           :target (file "project/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("i" "invoice" plain
           ,(format "#+title: %%<%%Y%%m%%d>-${title}\n%%[%s/template/invoice.org]" org-roam-directory)
           :target (file "invoice/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("f" "ref" plain
           ,(format "#+title: ${title}\n%%[%s/template/ref.org]" org-roam-directory)
           :target (file "ref/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("w" "works" plain
           ,(format "#+title: ${title}\n%%[%s/template/works.org]" org-roam-directory)
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("s" "secret" plain "#+title: ${title}\n\n"
           :target (file "secret/%<%Y%m%d%H%M%S>-${slug}.org.gpg")
           :unnarrowed t)
          ("h" "howdoyou" plain
           ,(format "#+title: ${title}\n%%[%s/template/howdoyou.org]" org-roam-directory)
           :target (file "howdoyou/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n")))
        org-roam-capture-ref-templates
        '(("l" "Web site" plain (function org-roam-capture--get-point)
           "${body}\n%?"
           :file-name "%<%Y%m%d>-${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+roam_key: ${ref}\n\n"
           :unnarrowed t)))
  (add-to-list 'org-roam-completion-functions #'org-roam-complete-tag-at-point)
  (add-hook 'org-roam-find-file-hook #'org-roam-update-slug-on-save-h)
  (add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)
  (advice-add #'org-roam-backlinks-section :override #'org-roam-grouped-backlinks-section)
  (advice-add #'org-roam-node-visit :around #'+popup-save-a)
  ;; (advice-add #'org-roam-node-list :filter-return #'org-roam-restore-insertion-order-for-tags-a)
  ;; (advice-remove #'org-roam-node-list #'org-roam-restore-insertion-order-for-tags-a) ; hotfix
  (advice-add #'org-roam-buffer-set-header-line-format :after #'org-roam-add-preamble-a))

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
(after! magit
  ;; Don't restore wconf after quit magit
  (setq magit-inhibit-save-previous-winconf t))
(after! forge
  (when EMACS29+ ; sqlite buitin support
    (setq forge-database-connector 'sqlite-builtin)))

;; Custom status to indicate that the service is doing something
;; that means it's not ready yet (compiling, etc)
(prodigy-define-status :id 'working :face 'prodigy-yellow-face)

;; Keep track of how many services are in each status
(defvar my-prodigy-service-counts nil "Map of status ID to number of
Prodigy processes with that status")

;; Poor man's hook: when a service's status changes, update the counts
;; in our alist
(advice-add
 'prodigy-set-status
 :before
 (lambda (service new-status)
   (let* ((old-status (plist-get service :status))
          (old-status-count (or 0 (alist-get old-status my-prodigy-service-counts)))
          (new-status-count (or 0 (alist-get new-status my-prodigy-service-counts))))
     (when old-status
       (setf (alist-get old-status my-prodigy-service-counts) (max 0 (- old-status-count 1))))
     (setf (alist-get new-status my-prodigy-service-counts) (+ 1 new-status-count))
     (force-mode-line-update t))))

(defun my-prodigy-working-count ()
  "Number of services with the 'working' status."
  (let ((count (alist-get 'working my-prodigy-service-counts 0)))
    (when (> count 0)
      (format "W:%d" count))))

(defun my-prodigy-failed-count ()
  "Number of services with the 'failed' status."
  (let ((count (alist-get 'failed my-prodigy-service-counts 0)))
    (if (> count 0)
        (format "F:%d" count)
      "")))

;; Show some status counts in the mode line so I can easily see when a
;; service is compiling or failed
(setq-default mode-line-format         ; TODO Change this to doom-modeline
              (append mode-line-format
                      '((:eval (my-prodigy-working-count))
                        (:eval (propertize (my-prodigy-failed-count)
                                           'face 'prodigy-red-face)))))

;; Compiling via yarn can take a long time, so detect when it
;; starts compiling and set the status to "working"
(prodigy-define-tag
  :name 'yarn
  :on-output (lambda (&rest args)
               (let ((output (plist-get args :output))
                     (service (plist-get args :service)))
                 (when (or (s-matches? "Already up to date." output)
                           (s-matches? "remote" output))
                   (prodigy-set-status service 'working)))))

;; Define a service that compiles a frontend app, using Browserify
(prodigy-define-service
  :name "my-frontend-app"
  :command "fish"
  :args '(".local/bin/run-fe.fish")
  :cwd (getenv "HOME")
  :ready-message "Server start at "
  :tags '(work yarn)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

;; (define more services here...)

(prodigy-define-service
  :name "PHP-FPM"
  :command "php-fpm"
  :args '("--nodaemonize")
  :tags '(work php)
  :stop-signal 'kill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "mysqld daemon"
  :command "mysqld_safe"
  :args '("--datadir=/opt/homebrew/var/mysql")
  :tags '(work mysql)
  :stop-signal 'kill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Redis server"
  :command "redis-server"
  :tags '(work redis)
  :stop-signal 'kill
  :kill-process-buffer-on-stop t)

;;
;;; Local Variables
(put 'flycheck-textlint-executable 'safe-local-variable #'stringp)
(put 'quickrun-option-command      'safe-local-variable #'stringp)
