;;; ~/.doom.d/functions.el -*- lexical-binding: t; -*-

(defun delete-surrounding-whitespace ()
  (interactive)
  (let ((skip-chars "\t\n\r "))
    (skip-chars-backward skip-chars)
    (let* ((start (point))
           (num (progn
                  (skip-chars-forward skip-chars)
                  (point))))
      (delete-region start num))))
