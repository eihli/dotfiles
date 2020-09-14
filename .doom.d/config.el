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
(setq doom-font (font-spec :family "monospace" :size 14))

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

(setq doom-unicode-font (font-spec :family "FiraCodeNerdFontCompleteM Nerd Font"
                                   :subfamily "Regular"
                                   :size 14))
(use-package! flycheck-clj-kondo)

(use-package! clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (require 'ob-clojure)
  (setq cider-clojure-cli-global-options "-A:cider-nrepl"
        cider-required-middleware-version "0.24.0"
        cider-jack-in-auto-inject-clojure nil
        cider-jack-in-lein-plugins nil
        org-babel-clojure-backend 'cider))

(use-package! paredit)

(use-package! elpy
  :init
  (advice-add 'python-mode :before #'elpy-enable)
  (setq-default elpy-rpc-pythonpath "/home/eihli/src/eihli-elpy"))

(use-package! web-mode
  :mode ("\\.html\\'" . web-mode))

(add-hook! web-mode
  (setq indent-tabs-mode t)
  (web-mode-use-tabs)
  (emmet-mode))

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

(add-hook! (clojure-mode lisp-mode)
           '(paredit-mode
             (lambda () (require 'flycheck-clj-kondo))))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(use-package! tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
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

(setq sly-complete-symbol-function 'sly-flex-completions)

(setq-default truncate-lines nil)

(load! "functions")
(load! "bindings")
