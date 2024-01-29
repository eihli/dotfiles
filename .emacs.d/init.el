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

(unless (package-installed-p 'inf-clojure)
  (package-refresh-contents)
  (package-install 'inf-clojure))


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

;; Custom might set some personal settings, like file paths and dev
;; tool options, that we don't want committed to get. Defining
;; custom-file here will make Custom write to that file rather than
;; our init.el
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(require 'clj-refactor)
(require 'ripgrep)
(require 'unicode-fonts) ;; Get Symbola http://users.teilar.gr/~g1951d/
(require 'helm-config)
(require 'sr-speedbar)
(require 'helm-projectile)

(helm-projectile-on)
(unicode-fonts-setup)
(helm-mode 1)
(global-nlinum-mode t)  ;; nlinum way better perf than linum
(show-paren-mode)
(electric-pair-mode 1)
(projectile-mode +1)

(setq-default helm-completion-style 'helm-fuzzy)
(setq-default helm-mode-fuzzy-match t)
(setq-default helm-completion-in-region-fuzzy-match t)

;; Allow passing args
(defun ow-rg (regexp &optional args)
  (ripgrep-regexp regexp (projectile-project-root) args))

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
  (company-mode +1)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist))))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

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

(defun my-toggle-hideshow-all () "Toggle hideshow all."
       (interactive)
       (setq my-hs-hide (not my-hs-hide))
       (if my-hs-hide
		   (hs-hide-all)
		 (hs-show-all)))

;; Fix shell path on Mac
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(put 'scroll-left 'disabled nil)

(when (file-exists-p "~/.emacs.d/env.el")
  (load "~/.emacs.d/env.el"))

(defun node-repl ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(defun save-line-reference-to-clipboard ()
  (interactive)
  (kill-new (format "%s:%s" (buffer-file-name) (line-number-at-pos))))
(global-set-key (kbd "C-c o l") 'save-line-reference-to-clipboard)

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

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'python-mode-hook
		  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'sgml-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)))
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'after-init-hook 'global-company-mode)

(setq js-indent-level 2)
;; (setq inf-clojure-generic-cmd "plk -d")

(setq-default inf-clojure-repl-type 'clojure)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
(setq-default css-indent-offset 2)
(setq-default cperl-indent-level tab-width)
(setq-default magit-save-repository-buffers 'dontask)
(setq-default truncate-lines nil)
(setq speedbar-show-unknown-files t
      sr-speedbar-width 40)
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)


(define-key projectile-mode-map (kbd "C-c p")
  'projectile-command-map)
(global-set-key (kbd "M-p") 'company-complete)
(global-set-key (kbd "M-n") 'completion-at-point)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c o r g") 'ow-rg)
(global-set-key (kbd "C-c o n") 'newline-each-element-in-list)
(global-set-key (kbd "C-c o b") 'python-shell-send-buffer)
(global-set-key (kbd "C-c h a") 'my-toggle-hideshow-all)
(global-set-key (kbd "C-c h l") 'hs-hide-level)
(global-set-key (kbd "C-c h t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x f") #'helm-find-files)
(global-set-key (kbd "C-c o f t") #'helm-gtags-find-tag)
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "C-t") 'sr-speedbar-toggle)


(require 'flx-ido)
(ido-mode 1)
;; (ido-everywhere 1) ;; Unable to use while helm-mode is enabled
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
