;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :completion
       (company          ; the ultimate code completion backend
        +childframe)     ; displaying completion candidates in a child frame
       (corfu +orderless); Completion Overlay Region FUnction
       (vertico +icons)  ; the search engine of the future

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +ascii     ; Include plain text emojis like =:)=.
              +github    ; Include Github-style emojis like =:smile:=.
              +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       indent-guides     ; highlighted indent columns
       (ligatures
        +extra +fira)    ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       (modeline         ; snazzy, Atom-inspired modeline, plus API
        +nyan            ; Nyanyanyanyanyanyanya! ♬ ♫ ♪ ♩
        +light)          ; less featureful version of the modeline
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; ensure all buffers (*name*) are treated as popups
        +defaults)       ; default popup rules for a variety of buffers.
       tabs              ; a tab bar for Emacs
       (treemacs +lsp)   ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil
        +everywhere)     ; come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons     ; making dired pretty [functional]
              +ranger)
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       (undo             ; persistent, smarter undo for your inevitable mistakes
        +tree)           ; branching undo history and a visualizer for navigating
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax           ; tasing you for every semicolon you forget
        +childframe)
       (:if
        (executable-find
         "aspell") spell ; tasing you for misspelling mispelling
        (spell +flyspell))
       grammar           ; tasing grammar mistake every you make

       :tools
       brief             ; tldr +cheat-sh
       (debugger +lsp)   ; stepping through code, to help you add bugs
       (docker +lsp)
       editorconfig
       (eval +overlay)   ; run code, run (also, repls)
       fzf               ; Command-line fuzzy finder written in Go
       (lookup           ; navigate your code and its documentation
        +docsets         ; ...or in Dash docsets locally
        +dictionary)     ; word definition and thesaurus lookup functionality.
       (lsp              ; M-x vscode
        +peek)           ; =lsp-ui-peek= when looking up def and references
       (magit            ; a git porcelain for Emacs
        +forge)          ; interface with git forges
       make              ; run make tasks from Emacs
       (pass +auth)      ; password manager for nerds
       pdf               ; pdf enhancements
       rgb               ; creating color strings

       :os
       (:if IS-MAC macos); improve compatibility with macOS
       (tty +osc)        ; improve the terminal Emacs experience

       :lang
       (clojure +lsp)    ; java with a lisp
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (json +lsp)       ; At least it ain't XML
       (javascript +lsp) ; all(hope(abandon(ye(who(enter(here))))))
       (markdown +grip)  ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +noter           ; enhanced PDF notetaking
        +jupyter         ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        +gnuplot         ; who doesn't like pretty pictures
        +present         ; using org-mode for presentations
        +roam2)          ; wander around notes
       php               ; perl's insecure younger brother
       (python +lsp)     ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       (scheme +guile)   ; a fully conniving family of lisps
       (sh +lsp          ; she sells {ba,z,fi}sh shells on the C xor
           +powershell)  ; the Microsoft shell
       (web +lsp)        ; the tubes
       (yaml +lsp)       ; JSON, but readable

       :email
       (mu4e +org +gmail)

       :app
       calendar
       ;;emms            ; The Emacs Multimedia System
       edit-server       ; Edit with Emacs server
       everywhere        ; *leave* Emacs!? You must be joking
       irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       twitter           ; twitter client https://twitter.com/vnought
       telega            ; telegram unofficial client

       :config
       literate
       (default +bindings +smartparens))
