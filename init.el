;;; package --- init.el Load the full configuration
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
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gpu-elpa"     . "https://elpa.gnu.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu-elpa"     . 5)
        ("org"          . 3)
        ("melpa"        . 6)))

;; don't load any packages before emacs starts up
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (require 'init-exec-path) ;; Set up $PATH
(eval-when-compile (require 'use-package))

;; keep packages updated
(use-package auto-package-update
  :defer t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; suppress byte compile warnings by turning them off
;; (setq ad-redefinition-action 'accept)

;; Keep the mode-line clean
(use-package diminish
  :ensure t)

;; modern files api for emacs
(use-package f
  :ensure t)

;; string manipulation library
(use-package s
  :ensure t)

(use-package dash
  :ensure t)

;; font-lock annotations like TODO in source code
(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode t))

;; allows to easily try new packages installing/uninstall automatically
(use-package try
  :ensure t)

;; flycheck on the fly checking code
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; formatting buffer on save according to autopep8
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;; sort python imports on buffer save according to length
(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save)
  (setq py-isort-options '("--length-sort")))

;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

;; ;; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind ("C-c m" . mc/edit-lines)
  :bind ("C-c C-n" . mc/mark-next-like-this)
  :bind ("C-c C-p" . mc/mark-previous-like-this)
  :bind ("C-c a" . mc/mark-all-like-this))

;; ;; expand region to select text like vim when using vi-()
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-c r") 'er/expand-region))

(use-package magit
  :pin melpa
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c M-g" . global-magit-file-mode)) ; C-x M-g magit dipatcher

;; LaTeX
(use-package tex
  :init
  (add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
  :hook ((LaTeX-mode-hook . reftex-mode)) ; with AUCTex LaTeX mode
  :hook ((latex-mode-hook . reftex-mode)) ; with Emacs latex mode
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-close-quote "")
  (setq TeX-open-quote "")
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t))

;; ;; (defun tex-view ()
;; ;;   (interactive)
;; ;;   (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)))

(use-package yasnippet
  :ensure t
  ;; :commands yas-load-directory
  :config
  ;; (setq yas-fallback-behavior 'return-nil)
  ;; (setq yas-also-auto-indent-first-line t)
  ;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; (add-hook 'html-mode-hook 'yas-minor-mode)
  (yas-load-directory (concat emacs-dir "snippets"))
  (yas-global-mode t))

(use-package pyvenv
  :ensure t)

;; ;; complete any, autocompletion package
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (global-company-mode t)
  ;; (global-set-key (kbd "C-<tab>") 'company-complete)
  )

;; ;; language server protocol for autocompletion, linting and documentation
(use-package lsp-mode
  :pin melpa
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (setq lsp-log-io t)
  ;; (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  )

;; ui interface to lsp for better completions and documentation
(use-package lsp-ui
  :pin melpa
  :ensure t
  :commands lsp-ui-mode
  ;; :config
  ;; (setq lsp-ui-doc-enable t)
  ;; (lsp-ui-doc-use-childframe t)
  ;; (setq lsp-ui-flycheck-enable t)
  ;; (lsp-ui-flycheck-list-position 'right)
  ;; (setq lsp-ui-flycheck-live-reporting t)
  ;; (setq lsp-ui-peek-enable t)
  ;; (lsp-ui-peek-list-width 60)
  ;; (lsp-ui-peek-peek-height 25)
  ;; (setq lsp-ui-doc-header t)
  ;; (setq lsp-ui-doc-include-signature t)
  ;; (lsp-ui-doc-position 'top)
  ;; (lsp-ui-doc-border (face-foreground 'default))
  ;; (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-ui-sideline-ignore-duplicate t)
  ;; (setq lsp-ui-sideline-show-code-actions t)
  )

;; package providing completions for lsp through company
(use-package company-lsp
  :pin melpa
  :ensure t
  :commands company-lsp
  :config (push 'company-lsp company-backends)
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

;; let's use microsoft's language server protocol since it's faster than pyls
;; using this doesn't require pip install pyls
(use-package lsp-python-ms
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp))))  ; or lsp-deferred

;; debugger adapter protocol for many languages
;; don't forget to install ptvsd on your python venv environment
(use-package dap-mode
  :ensure t
  :config
  (setq dap-mode t)
  (setq dap-ui-mode t)
  ;; enables mouse hover support
  (setq dap-tooltip-mode t)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (setq tooltip-mode t)
  (require 'dap-python))

;; ;; (use-package lsp-ui
;; ;;   :requires lsp-mode flycheck
;; ;;   :commands lsp-ui-mode
;; ;;   :custom-face
;; ;;   (lsp-ui-doc-background ((t (:background nil))))
;; ;;   (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;; ;;   :bind (:map lsp-ui-mode-map
;; ;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;; ;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;; ;;               ("C-c u" . lsp-ui-imenu))
;; ;;   :hook (lsp-mode-hook . lsp-ui-mode)
;; ;;   :custom
;; ;;   (lsp-ui-doc-enable t)
;; ;;   (lsp-ui-doc-use-childframe t)
;; ;;   (lsp-ui-flycheck-enable t)
;; ;;   (lsp-ui-flycheck-list-position 'right)
;; ;;   (lsp-ui-flycheck-live-reporting t)
;; ;;   (lsp-ui-peek-enable t)
;; ;;   (lsp-ui-peek-list-width 60)
;; ;;   (lsp-ui-peek-peek-height 25)
;; ;;   (lsp-ui-doc-header t)
;; ;;   (lsp-ui-doc-include-signature t)
;; ;;   (lsp-ui-doc-position 'top)
;; ;;   (lsp-ui-doc-border (face-foreground 'default))
;; ;;   (lsp-ui-sideline-enable t)
;; ;;   (lsp-ui-sideline-ignore-duplicate t)
;; ;;   (lsp-ui-sideline-show-code-actions t)
;; ;;   :config
;; ;;   ;; Use lsp-ui-doc-webkit only in GUI
;; ;;   (setq lsp-ui-doc-use-webkit nil)
;; ;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;; ;;   ;; emacs-lsp/lsp-ui#243
;; ;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;; ;;     (setq mode-line-format nil)))

(use-package highlight-indent-guides
  :defines highlight-indent-guides-mode
  :commands highlight-indent-guides-mode
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

;; On Linux Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
;; import env vars if you are using macOS.
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

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
