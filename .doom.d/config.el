;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Eric Ihli"
      user-mail-address "eihli@owoga.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; 24 for hidpi
;; 12 for lodpi
(setq doom-font (font-spec :family "FiraCodeNerdFontCompleteM Nerd Font" :size 12))
(setq doom-unicode-font (font-spec :family "Noto Color Emoji" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(use-package! adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))
(use-package! stumpwm-mode)
(use-package! smart-tabs-mode
  :config
  (autoload 'smart-tabs-mode "smart-tabs-mode")
  (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
  (autoload 'smart-tabs-advice "smart-tabs-mode")
  (autoload 'smart-tabs-insinuate "smart-tabs-mode"))

(use-package! flycheck-clj-kondo)

(use-package! extempore-mode
  :config
  (paredit-mode))

(use-package! clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (require 'ob-clojure)
  (setq cider-clojure-cli-global-options "-A:cider-nrepl"
        cider-required-middleware-version "0.24.0"
        cider-jack-in-auto-inject-clojure nil
        cider-jack-in-lein-plugins nil
        org-babel-clojure-backend 'cider))

;; (use-package! geiser
;;   :config
;;   (setq geiser-active-implementations '(chez))
;;   (setq geiser-chez-binary "chez")
;;   (setq geiser-chez--prompt-regexp ".*>.*"))

(use-package! paredit)

(after! js-mode
  (setq js-indent-level 2))

(use-package! web-mode
  :mode ("\\.html\\'" . web-mode)
  :config
  (setq-default tab-width 2)
  (web-mode-use-tabs)
  (emmet-mode)
  (setq-default electric-indent-mode nil))

(use-package! hcl-mode
  :mode (("\\.hcl\\.j2\\'" . hcl-mode)
         ("\\.hcl\\'" . hcl-mode)))

(use-package! seq
  :ensure t)
(use-package! clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (set-lookup-handlers! 'clj-refactor-mode
    :references #'cljr-find-usages)
  (map! :map clojure-mode-map
        :localleader
        :desc "refactor" "R" #'hydra-cljr-help-menu/body))

(after! clojure
  (define-clojure-indent
    (>defn :defn)
    (defsc :defn)
    (action :defn)
    (defmutation :defn)))

;; (add-hook! (racket-mode)
;;            '(paredit-mode))

(add-hook! (clojure-mode lisp-mode)
           '(paredit-mode (lambda () (require 'flycheck-clj-kondo)))
           (setq tab-width 8))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(use-package! jupyter-ascending)

(use-package! tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checker 'javascript-eslint)
  (setq tab-width 4)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package! json-mode)

;; (use-package! lsp-grammarly
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp))))  ; or lsp-deferred

(setq sly-complete-symbol-function 'sly-flex-completions)

(setq-default truncate-lines nil)


(map!
 :map rst-mode-map
 :leader
 (:prefix "r"
  :n
  "t" #'rst-toc
  "s" #'rst-toc-follow-link))

(load! "functions")
(load! "bindings")

(server-start)
