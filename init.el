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

;; authentication without user interaction for async if using mail from emacs
;; (use-package auth-source
;;   :no-require t
;;   :config (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))

;; allows to easily try new packages installing/uninstall automatically
(use-package try
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup for company-jedi but seems slow in providing completions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company for autocompletion
;; (use-package company
;;   :ensure t
;;   :init (global-company-mode t)
;;   :config
;;   (setq company-idle-delay 0)
;;   (setq company-begin-commands '(self-insert-command)))

;; ;; use the melpa since melpa-stable apparently has bugs
;; (use-package jedi-core
;;   :pin melpa
;;   :ensure t)

;; ;; there's an issue with func signatures
;; ;; needs to add changes manually https://github.com/syohex/emacs-company-jedi/issues/24
;; ;; requires jedi, virtualenv, and epc installed via pip in your env outside emacs
;; (use-package company-jedi
;;   :ensure t
;;   :init (add-to-list 'company-backends 'company-jedi)
;;   :config (setq jedi:complete-on-dot t))

;; (use-package python
;;   :hook ((python-mode . jedi:setup)))

;; flycheck on the fly checking code
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;; undo-tree
(use-package undo-tree
  :ensure t
  ;; :defines undo-tree-visualizer-selection-mode
  :config
  (defvar undo-tree-mode)
  (defvar undo-tree-visualizer-selection-node)
  (global-undo-tree-mode t))
  ;; (diminish 'undo-tree-mode)))  ;; sensible undo-tree

;; deletes all the whitespace when you hit backspace or delete
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

;; expand region to select text like vim when using vi-()
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-c r") 'er/expand-region))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; LaTeX
(use-package tex
  :defines TeX-test-compilation-log
  :functions AUCTeX-set-ert-path
  :ensure auctex)

;; (defun tex-view ()
;;   (interactive)
;;   (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  :bind ("C-c p" . projectile-command-map))

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
  ;; :hook ((python-mode . pyvenv-mode))

(use-package elpy
  :ensure t
  :init (elpy-enable t))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup for anaconda-mode if company/jedi are not preferred ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package company
;;   :defer t
;;   :bind (:map company-active-map
;;               ("RET"     . company-complete-selection)
;;               ([return]  . company-complete-selection)
;;               ("TAB"     . company-select-next)
;;               ([tab]     . company-select-next)
;;               ("S-TAB"   . company-select-previous)
;;               ([backtab] . company-select-previous)
;;               ("C-j"     . company-complete-selection))
;;   :config
;;   (setq company-idle-delay 0)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 1)
;;   ;; Aligns annotation to the right hand side
;;   ;; (setq company-tooltip-align-annotations t)
;;   (setq company-begin-commands '(self-insert-command))
;;   (global-company-mode t))

;; (use-package anaconda-mode
;;   :ensure t
;;   :bind (:map anaconda-mode-map
;;                 ("M-," . anaconda-mode-find-assignments))
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode)))

;; (use-package company-anaconda
;;   :ensure t
;;   :config (add-to-list 'company-backends 'company-anaconda))

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
