
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories ("node_modules"))
			    (files ("*~"))))

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (autopair auto-complete fiplr)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (require
	    (quote find-file-in-project))
	   (seq ffip-prune-patterns
		(\`
		 ("*/python-environments/*"
		  (\,@ ffip-prune-patterns)))))
     (ffip-find-options . "-not -size +64k -not -iwholename 'db.sqlite'")
     (fiplr-ignored-globs quote
			  ((directories
			    ("python-environments"))))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(elpy-enable)
(load "elpy")
(define-key elpy-mode-map (kbd "M-n") 'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "M-p") 'elpy-nav-backward-block)

(autopair-global-mode)

(add-hook 'python-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(setq js-indent-level 2)
