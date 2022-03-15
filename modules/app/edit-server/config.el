;;; app/edit-server/config.el -*- lexical-binding: t; -*-

(use-package! edit-server
  :hook (after-init . edit-server-start)
  :config (setq edit-server-new-frame-alist
                `((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . ,(if (featurep 'pgtk) 'pgtk 'x))))
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode))))
