;;; package --- Summary

;; init.el -*- lexical-binding: t; -*-
;;; init.el --- Load the full configuration
;;; Commentary:

;;; Code:
;; produce backtraces when errors occur

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq debug-on-error t)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

;; setup directories
(defvar emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs.")
(defvar config-dir (expand-file-name "config" emacs-dir)
  "Personal configuration dir.")
(defvar bin-dir (expand-file-name "bin" emacs-dir)
  "This directory houses 3rd party software.")
(defvar savefile-dir (expand-file-name "savefile" emacs-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(unless (file-exists-p bin-dir)
  (make-directory bin-dir))

(defun add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (add-subfolders-to-load-path name)))))

;; add directories to Emacs's `load-path'
;; (add-to-list 'load-path config-dir)
(add-to-list 'load-path bin-dir)
(add-subfolders-to-load-path bin-dir)

;; ;; reduce the frequency of garbage collection by making it happen on
;; ;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; ;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "init-custom.el" config-dir))

;; load the personal settings (this includes `custom-file')
;; (when (file-exists-p config-dir)
;;   (message "Loading personal configuration files in %s..." config-dir)
;;   (mapc 'load (directory-files config-dir 't "^[^#\.].*\\.el$")))

;; (require 'global-keybindings)

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper))  ;; I'ts all in the Meta

;; setup package archives
(require 'package)
(setq package-archives
      '(("MELPA STABLE" . "https://stable.melpa.org/packages/")
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("ORG"          . 3)
        ("MELPA"        . 1)))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Keep the mode-line clean
(use-package diminish
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

;; columnd indicator at 80 chars for code writing
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-color "orange")
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode))

;; auto-complete for autocompletion
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;     (ac-config-default)
;;     (global-auto-complete-mode t)))

;; company for autocompletion
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  ;; (setq company-show-numbers t)
  ;; (setq company-tooltip-limit 20)
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case t)
  ;; (setq company-dabbrev-code-ignore-case t)
  ;; (setq company-dabbrev-code-everywhere t)
  ;; (setq company-etags-ignore-case t)
  ;; (setq company-global-modes
  ;;       '(not
  ;;         eshell-mode comint-mode text-mode erc-mode))
  (global-company-mode t))

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup))

;; flycheck on the fly checking code
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; ;; jedi for python auto-completion
;; (use-package jedi
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup))

;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))
  ;; (diminish 'undo-tree-mode)))  ;; sensible undo-tree

;; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t)

;; expand region to select text like vim when using vi-()
(use-package expand-region
  :ensure t
  :config
 ( global-set-key (kbd "C-c r") 'er/expand-region))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; LaTeX
(use-package tex
  :ensure auctex
  :defer t)

;; (defun tex-view ()
;;   (interactive)
;;   (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  :bind ("C-c p" . projectile-command-map))

;; allows to easily try new packages installing/uninstall automatically
(use-package try
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  ;; (setq yas-fallback-behavior 'return-nil)
  ;; (setq yas-also-auto-indent-first-line t)
  ;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; (add-hook 'html-mode-hook 'yas-minor-mode)
  (yas-load-directory (concat emacs-dir "snippets"))
  (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure t)

(use-package s
  :ensure t)

(use-package pyvenv
  :ensure t)

;; another change here

;; brings ivy as dependency
;; (use-package find-file-in-project
;;   :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

;; (use-package elpy
;;   :ensure t
;;   :custom (elpy-rpc-backend "jedi")
;;   :config (elpy-enable))

;; On Linux Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; ace-window allows to jump between windows with C-x o winnumber
;; (use-package ace-window
;;   :ensure t
;;   :init
;;   (progn
;;     (global-set-key [remap other-window] 'ace-window)
;;     (custom-set-faces
;;      '(aw-leading-char-face
;;        ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p config-dir)
  (message "Loading personal configuration files in %s..." config-dir)
  (mapc 'load (directory-files config-dir 't "^[^#\.].*\\.el$")))

(message "Emacs is ready Master %s!" current-user)

(provide 'init)
;;; init.el ends here
