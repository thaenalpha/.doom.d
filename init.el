;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :completion
       (corfu +orderless   ; Completion Overlay Region FUnction
              +tabnine     ; cape-tabnine-to-capf
              +childframe) ; Documentation popup for Corfu
       (vertico            ; the search engine of the future
        +childframe        ; Using posframe to show Vertico
        +icons)

       :ui
       discover            ; discover more of Emacs
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       (emoji +ascii       ; include plain text emojis like =:)=.
              +github      ; include Github-style emojis like =:smile:=.
              +unicode)    ; 🙂
       fixmee              ; quickly navigate to FIXME notices in code
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       minimap             ; show a map of the code on the side
       (modeline           ; snazzy, Atom-inspired modeline, plus API
        +nyan)             ; nyanyanyanyanyanyanya! ♬ ♫ ♪ ♩
       nav-flash           ; blink cursor line after big motions
       ophints             ; highlight the region an operation acts on
       (popup              ; tame sudden yet inevitable temporary windows
        +defaults)         ; default popup rules for a variety of buffers.
       (treemacs +lsp)     ; a project drawer, like neotree but cooler
       vc-gutter           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       window-select       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       zen                 ; distraction-free coding or writing

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       format              ; automated prettiness
       multiple-cursors    ; editing in many places at once
       parinfer            ; turn lisp into python, sort of
       rotate-text         ; cycle region at point between text candidates
       snippets            ; my elves. They type so I don't have to
       substitute          ; efficiently replace targets in the buffer or context

       :emacs
       ctrlf               ; Emacs finally learns how to ctrl+F
       (dired +icons       ; making dired pretty [functional]
        +dirvish)          ; A modern file manager based on dired mode
       electric            ; smarter, keyword-based electric-indent
       (ibuffer +icons)    ; interactive buffer management
       info                ; Info package for Emacs
       (undo               ; persistent, smarter undo for your inevitable mistakes
        +tree)             ; branching undo history and a visualizer for navigating
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       eshell              ; the elisp shell that works everywhere
       vterm               ; the best terminal emulation in Emacs

       :checkers
       (syntax +childframe); tasing you for every semicolon you forget
       (:if (executable-find
             "aspell")
        spell              ; tasing you for misspelling mispelling
        (spell +flyspell))
       grammar             ; tasing grammar mistake every you make

       :tools
       biblio              ; Writes a PhD for you (citation needed)
       brief               ; tldr +cheat-sh
       (debugger +lsp)     ; stepping through code, to help you add bugs
       direnv              ; integrates direnv into Emacs.
       (docker +lsp)       ; Emacs interface to Docker
       editorconfig        ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       fzf                 ; Command-line fuzzy finder written in Go
       gist                ; interacting with github gists
       (lookup             ; navigate your code and its documentation
        +docsets           ; …or in Dash docsets locally
        +devdocs           ; another API documentation viewer for Emacs
        +dictionary)       ; word definition and thesaurus lookup functionality.
       lsp                 ; Language Server Protocol
       (magit              ; a Git porcelain inside Emacs
        +forge)            ; interface with git forges
       make                ; run make tasks from Emacs
       (pass +auth)        ; password manager for nerds
       pdf                 ; pdf enhancements
       prodigy             ; managing external services & code builders
       rgb                 ; creating color strings
       tmux                ; an API for interacting with tmux
       tree-sitter         ; syntax and parsing, sitting in a tree...

       :os
       (:if IS-MAC macos   ; improve compatibility with macOS
        (:if
         (getenv "WSLENV") ; "WT_SESSION::WT_PROFILE_ID"
         wsl))             ; improve compatibility with WSL
       (tty +osc)          ; improve the terminal Emacs experience

       :lang
       (cc +lsp            ; C > C++ == 1
           +tree-sitter)
       (clojure +lsp)      ; java with a lisp
       data                ; config/data formats
       emacs-lisp          ; drown in parentheses
       (graphql +lsp)      ; Give queries a REST
       (javascript         ; all(hope(abandon(ye(who(enter(here))))))
        +lsp +tree-sitter)
       (json +lsp          ; At least it ain't XML
             +tree-sitter)
       (lua +lsp)          ; one-based indices? one-based indices
       (markdown +grip)    ; writing docs for people to ignore
       (nix +lsp           ; I hereby declare "nix geht mehr!"
            +tree-sitter)
       (org                ; organize your plain life in plain text
        +dragndrop         ; drag & drop files/images into org buffers
        +gnuplot           ; who doesn't like pretty pictures
        +noter             ; enhanced PDF notetaking
        +pandoc            ; export-with-pandoc support
        +passwords         ; A password manager for Org.
        +pomodoro          ; timer for clocking time on tasks.
        +present           ; using org-mode for presentations
        +roam2             ; wander around notes
        +web)              ; Display and capture web content with Org-mode
       (php +tree-sitter)  ; perl's insecure younger brother
       plantuml            ; diagrams for confusing people more
       (python +tree-sitter; beautiful is better than ugly
        +lsp +pyright)
       (rest +jq)          ; Emacs as a REST client
       (rust +lsp)         ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh                 ; she sells {ba,z,fi}sh shells on the C xor
        +fish)             ; non-posix, but readable
       (web +lsp           ; the tubes
            +tailwind      ; write styles you want from html
            +tree-sitter)
       (yaml +lsp)         ; JSON, but readable

       :email
       (mu4e +org +gmail)

       :app
       calendar
       edit-server         ; Edit with Emacs server
       slack               ; Slack client for Emacs
       telega              ; telegram unofficial client
       (:unless (or IS-WINDOWS
                    (getenv "WSLENV"))
        everywhere)        ; *leave* Emacs!? You must be joking
       jira                ; Syncing between Jira and Org-mode.
       leetcode            ; Leetcode integration
       mastodon            ; Client for Mastodon
       pocket              ; Client for Pocket reading list https://getpocket.com
       reddit              ; read from Emacs
       (rss +org)          ; emacs as an RSS reader
       twitter             ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens))
