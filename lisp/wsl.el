;;; lisp/wsl.el -*- lexical-binding: t; -*- WSL package to interactive with Windows's programs, such as browsing the URL with Chrome, open the PDF file with Acrobat Reader DC, open the current file with default program, launch explorer.exe, etc.
(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(defun wsl-browse-url-xdg-open (url &optional ignored)
  (interactive (browse-url-interactive-arg "URL: "))
  (shell-command-to-string (concat "explorer.exe " url)))
(advice-add #'browse-url-xdg-open :override #'wsl-browse-url-xdg-open)

(provide 'wsl)

;;; wsl.el ends here
