;;; lisp/arrayify.el -*- lexical-binding: t; -*-
;;;###autoload
(defun arrayify (start end quote join)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: \nMJoin: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) join)))
    (delete-region start end)
    (insert insertion)))

;; josh
;; sam
;; jed
;; C.J.
;; toby
;; => "josh", "sam", "jed", "C.J.", "toby"
(provide 'arrayify)
;;; arrayify.el ends here
