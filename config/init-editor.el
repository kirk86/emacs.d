;;; package --- Summary
;; init.el -*- lexical-binding: t; -*-
;;; init-editor.el --- Load the full configuration
;;; Commentary:

;;; Code:
;; Package to change default highlight colour
(require 'hl-line)

;; turn off default message screens with emacs tutorial
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Customize scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; Make scrolling quicker
(setq auto-window-vscroll nil)

;; Dont jump when scrolling by line
(setq scroll-conservatively 10)

;; UTF-8 as default encoding
(prefer-coding-system 'utf-8)
(setq system-time-locale "en_US.utf8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

;; Disable menu bar
(menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; Enable line numbers
;;(linum-mode)  ;; enables only for current buffer
(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook 'linum-mode))
  ;;(global-linum-mode t))

;; Add solid line after line number
(setq linum-format "%4d \u2502 ")

;; Show matching parentheses
(show-paren-mode t)

;; Insert matcing parentheses
(electric-pair-mode t)

;; Make auto-indentation globally availabe
(electric-indent-mode t)

;; Treat region like typical text selection
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; revert buffers automatically when underlying files are changed
;; externally
(global-auto-revert-mode t)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; projectile is a project management mode
;; (require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
;; (projectile-mode t)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" savefile-dir))

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
;; (savehist-mode +1)

;; Setup tabs and spaces

;; Use only spaces and not tabs
(setq-default indent-tabs-mode nil)
;; Default tab char display width 4 spaces
(setq-default tab-width 4)
;; Highlight tabs and spaces
(setq-default highlight-tabs t)
(setq-default highlight-trailing-whitespace t)
;; Set background highlight colour to green
(set-face-attribute 'region nil :background "#109540")
;; Don't highlight the end of long lines
(setq whitespace-line-column 99999)

;; enable ido mode
;; (setq indo-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode t)

;; enable window configuration back/forth C-c <-/->
;; (winner-mode t)

;; windmove enables moving between windows with S - <-/->
(windmove-default-keybindings)

;; Set window title to full file name
(setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f"))

;; set custom-set-variables/faces
;; (setq custom-file "~/.emacs.d/personal/init-custom.el")

;; Setup stuff on macOS
;; (when-system darwin
;;   ;; Change behavior of left command key
;;   (setq mac-command-modifier 'meta)
             
;;   ;; Fix dired not working
;;   (require 'ls-lisp)
;;   (setq ls-lisp-dirs-first t
;; 	ls-lisp-use-insert-directory-program nil)
;;   (setq dired-listing-switches "-alhv")

;;   ;; Add brew binaries to PATH
;;   (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;   (add-to-list 'exec-path "/usr/local/bin")

;;   ;; Disable bell
;;   (setq ring-bell-function 'ignore)

;;   ;; Setup ispell
;;   (setq ispell-program-name "/usr/local/bin/aspell"))

;; ;; It's all in the Meta
;; (setq ns-function-modifier 'hyper)

(provide 'init-editor)
;;; init-editor.el ends here
