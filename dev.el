(defun my-comint-shorten-long-lines (text)
  (let* ((regexp "^\\(.\\{40\\}\\).*?\\(.\\{40\\}\\)$")
         (shortened-text
		  (replace-regexp-in-string
		   regexp
		   "\\1<...>\\2"
		   text)))
	shortened-text))

(add-hook 'comint-preoutput-filter-functions 'my-comint-shorten-long-lines)
