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

(defun lotto-cljs ()
  (interactive)
  (cider-connect-cljs '(:host "localhost" :port 9001 :cljs-repl-type shadow)))

(defun copy-buffer-filename-to-clipboard ()
  (interactive)
  (kill-new (buffer-file-name)))

(defun httpsify (url)
  (if (string-prefix-p "git@" url)
      (replace-regexp-in-string
       ".com:"
       ".com/"
       (replace-regexp-in-string "git@" "https://" url))
    url))

(defun clipboard-github-link-from-current-line ()
  (interactive)
  (let* ((base-url (httpsify (substring (magit-get "remote" "origin" "url") 0 -4)))
         (project-file-name (replace-regexp-in-string
                             (counsel-locate-git-root)
                             ""
                             (buffer-file-name)))
         (url-format "%s/blob/%s/%s#L%s"))
    (kill-new (format url-format base-url (magit-get-current-branch) project-file-name (line-number-at-pos)))))

(defun clipboard-man7 (manpage)
  (interactive "sCopy to clipboard manpage link for: ")
  (destructuring-bind (manpage word) (split-string manpage)
    (kill-new (format "https://man7.org/linux/man-pages/man%s/%s.%s.html" manpage word manpage))))
