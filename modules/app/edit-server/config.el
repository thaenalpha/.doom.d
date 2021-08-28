;;; app/edit-server/config.el -*- lexical-binding: t; -*-

(use-package! edit-server
  :defer t
  :commands edit-server-start
  :init (edit-server-start)
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . x))))
