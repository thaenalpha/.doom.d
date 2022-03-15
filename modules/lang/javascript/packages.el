;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! rjsx-mode)
(package! typescript-mode)

;; Tools
(package! js2-refactor)
(package! npm-mode)
(package! add-node-modules-path)
(package! pnpm-mode)
(package! import-js)
(package! yarn :recipe (:host github :repo "thaenalpha/yarn.el"))

;; Eval
(package! nodejs-repl)
(package! skewer-mode)

;; Programming environment
(package! tide)
(when (featurep! :tools lookup)
  (package! xref-js2))
