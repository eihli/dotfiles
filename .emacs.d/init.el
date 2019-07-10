;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)

;; Turn off that annoying beep in window mode
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(require 'seq)
;; Custom load path
;; Adds all subdirectories under .emacs.d/extensions
;; to the load-path.
(let* ((subdirs (seq-filter
				 (lambda (f)
				   (and (file-directory-p (concat "~/.emacs.d/extensions/" f))
						(not (or (equal f ".")
								 (equal f "..")))))
				 (directory-files "~/.emacs.d/extensions")))
       (subdir-paths (mapcar (lambda (f)
							   (concat "~/.emacs.d/extensions/" f))
							 subdirs)))
  (let (val)
    (dolist (el subdir-paths val)
      (add-to-list 'load-path el))))
(require 'node-ac-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-rpc-backend "rope" t)
 '(package-selected-packages
   (quote
	(helm-gtags exec-path-from-shell cljr-helm helm helm-cider helm-projectile paredit http-twiddle rainbow-delimiters clj-refactor ac-cider ripgrep flx-ido cider clojure-mode unicode-fonts yasnippet-classic-snippets js-format magit yasnippet-snippets elpy elpygen nlinum yasnippet indium graphviz-dot-mode ## markdown-mode yaml-mode projectile slime emmet-mode tide json-mode autopair auto-complete)))
 '(safe-local-variable-values
   (quote
	((eval progn
		   (require
			(quote find-file-in-project))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/node_modules/*"
				   (\,@ ffip-prune-patterns))))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/env/*"
				   (\,@ ffip-prune-patterns))))
		   (setq ffip-prune-patterns
				 (\`
				  ("*.csv"
				   (\,@ ffip-prune-patterns))))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/static/*"
				   (\,@ ffip-prune-patterns)))))
	 (eval progn
		   (require
			(quote find-file-in-project))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/node_modules/*"
				   (\,@ ffip-prune-patterns))))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/env/*"
				   (\,@ ffip-prune-patterns))))
		   (setq ffip-prune-patterns
				 (\`
				  ("*.csv"
				   (\,@ ffip-prune-patterns)))))
	 (eval progn
		   (require
			(quote find-file-in-project))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/env/*"
				   (\,@ ffip-prune-patterns))))
		   (setq ffip-prune-patterns
				 (\`
				  ("*.csv"
				   (\,@ ffip-prune-patterns)))))
	 (eval progn
		   (require
			(quote find-file-in-project))
		   (setq ffip-prune-patterns
				 (\`
				  ("*/python-environments/*"
				   (\,@ ffip-prune-patterns)))))
	 (eval progn
		   (require
			(quote find-file-in-project))
		   (seq ffip-prune-patterns
				(\`
				 ("*/python-environments/*"
				  (\,@ ffip-prune-patterns)))))
	 (ffip-find-options . "-not -size +64k -not -iwholename 'db.sqlite'")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(company-tooltip ((t (:foreground "yellow"))))
 '(region ((t (:background "dim gray" :distant-foreground "dark gray")))))

(elpy-enable)
(require 'elpy)
(define-key elpy-mode-map (kbd "M-n") 'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "M-p") 'elpy-nav-backward-block)

;; Use jedi for stuff
(setq elpy-rpc-backend "rope")

(require 'helm-config)
(helm-mode 1)

;; Fuzzy search
(global-set-key (kbd "C-x f") #'helm-find-files)

;; Tags searching
(global-set-key (kbd "C-c o f t") #'helm-gtags-find-tag)

;; Allow passing args
(defun ow-rg (regexp &optional args)
  (ripgrep-regexp regexp (projectile-project-root) args))

;; Interactive-do-things
(require 'flx-ido)
(ido-mode t)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Comment/uncomment line
(global-set-key (kbd "C-x ;") 'comment-line)

;; Buffer switching
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)


(add-hook 'python-mode-hook
		  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(setq js-indent-level 2)

(global-nlinum-mode t)  ;; nlinum way better perf than linum

;; TypeScript Stuff/Tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(setq typescript-indent-level 2)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; auto-complete default configuration and enable
(ac-config-default)

;; override indent.el so we have autocomplete
(global-set-key (kbd "M-q") 'company-complete)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq-default magit-save-repository-buffers 'dontask)

;; sr-speedbar
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t
      sr-speedbar-width 40)
(global-set-key (kbd "C-t") 'sr-speedbar-toggle)

;; Emmet mode for autocompleting html/markup
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'sgml-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)))
;; Fixes error running python through emacs
;; https://github.com/jorgenschaefer/elpy/issues/887
(setq python-shell-completion-native-enable nil)

;; Slime/Lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el")))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/home/eihli/bin/sbcl")


;; Don't clutter every folder with backups
(setq backup-directory-alist
      `((".*" . "~/.ebaks/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.ebaks/" t)))

;; Easier switching windows
(global-set-key (kbd "C-o") 'other-window)

(setq projectile-enable-caching t)

;; Speed up things like ffip
(setq gc-cons-threshold 20000000)

;; Enable markdown export for org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))


;; Enable python evaluation in code blocks in org mode
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))


;; hs-minor-mode and shortcuts
(add-hook 'prog-mode-hook 'hs-minor-mode)
(defvar my-hs-hide nil "Current state of hideshow for toggling all.")
;;;###autoload
(defun my-toggle-hideshow-all () "Toggle hideshow all."
       (interactive)
       (setq my-hs-hide (not my-hs-hide))
       (if my-hs-hide
		   (hs-hide-all)
		 (hs-show-all)))
(global-set-key (kbd "C-c h a") 'my-toggle-hideshow-all)
(global-set-key (kbd "C-c h l") 'hs-hide-level)
(global-set-key (kbd "C-c h t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c f") 'projectile-find-file)

(global-set-key (kbd "M-i") 'imenu)

;; Enable projectile mode
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Fix shell path on Mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Get Symbola http://users.teilar.gr/~g1951d/
(require 'unicode-fonts)
(unicode-fonts-setup)

;; Not using Noto Emoji because it's Google.
;; (set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(put 'scroll-left 'disabled nil)

(when (file-exists-p "~/.emacs.d/env.el")
  (load "~/.emacs.d/env.el"))

(show-paren-mode)
(electric-pair-mode 1)

(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
(setq-default cperl-indent-level tab-width)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(defun node-repl ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(exec-path-from-shell-initialize)

(defun save-line-reference-to-clipboard ()
  (interactive)
  (kill-new (format "%s:%s" (buffer-file-name) (line-number-at-pos))))
(global-set-key (kbd "C-c o l") 'save-line-reference-to-clipboard)

(require 'ripgrep)
(defun ow-rg (regex &optional dir args)
  (interactive
   (list (read-from-minibuffer "Regex: " (thing-at-point 'symbol))
		 (read-directory-name "Directory: " (projectile-project-root))
		 (read-from-minibuffer "args: ")))
  (let ((dir (if (string= dir "") (projectile-project-root) dir))
		(args (if (equal args "")
				  nil
				(first (read-from-string args)))))
    (if args
		(ripgrep-regexp
		 regex
		 dir
		 (mapcar 'shell-quote-argument args))
      (ripgrep-regexp
       regex
       dir))))

(global-set-key (kbd "C-c o r g") 'ow-rg)

(defun newline-each-element-in-list ()
  (interactive
   (let ((pairs '(("[" "]")
				  ("(" ")")
				  ("{" "}"))))
     (search-backward-regexp "[\[\(\{]")
     (let ((start (char-after (point)))
		   (end (string-to-char (cadr (assoc (match-string 0) pairs))))
		   (stack 1))
       (forward-char 1)
       (newline nil t)
       (while (or
			   (not (equal (char-after (point)) end))
			   (not (equal stack 0)))
		 (forward-char 1)
		 (cond ((char-equal (char-before (point)) start)
				(setq stack (+ stack 1)))
			   ((and (equal stack 1)
					 (char-equal (char-before (point)) ?,))
				(newline nil t))
			   ((char-equal (char-after (point)) end)
				(setq stack (- stack 1)))))
       (newline nil t)))))

(global-set-key (kbd "C-c o n") 'newline-each-element-in-list)
