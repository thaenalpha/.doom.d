;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! rjsx-mode :built-in 'prefer)
(package! typescript-mode :built-in 'prefer)

;; Tools
(package! js2-refactor :built-in 'prefer)
(package! npm-mode :built-in 'prefer)
(package! add-node-modules-path :built-in 'prefer)
(package! pnpm-mode)
(package! import-js)
(package! yarn
  :recipe (:host github :repo "thaenalpha/yarn.el"))

;; Eval
(package! nodejs-repl :built-in 'prefer)
(package! skewer-mode :built-in 'prefer)

;; Programming environment
(package! tide :built-in 'prefer)
(when (featurep! :tools lookup)
  (package! xref-js2 :built-in 'prefer))
