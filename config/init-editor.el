;;; package --- Summary
;; init.el -*- lexical-binding: t; -*-
;;; init-editor.el --- Load the full configuration
;;; Commentary:

;;; Code:

;;; include packages
(require 'hl-line)  ;; highlight line
(require 'eshell)
(require 'recentf)
(require 'savehist)

;; UI Settings

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)
;; (scroll-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
;; (blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; customize scratch buffer
;; turn off default message screens with emacs tutorial
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; ;; Make scrolling quicker
;; (setq auto-window-vscroll nil)

;; ;; Dont jump when scrolling by line
;; (setq scroll-conservatively 10)

;; mode line settings
;; (line-number-mode t)
;; (column-number-mode t)
;; (size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Prelude - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name
                    (buffer-file-name))
                 "%b"))))

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

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Newline at end of file
(setq require-final-newline t)

;; UTF-8 as default encoding
(prefer-coding-system 'utf-8)
(setq system-time-locale "en_US.utf8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

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

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)

;; save recent files
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun my-recentf-exclude-p (file)
"A predicate to decide whether to exclude FILE from recentf."
(let ((file-dir (file-truename (file-name-directory file))))
  (cl-some (lambda (dir)
             (string-prefix-p dir file-dir))
           (mapcar 'file-truename (list savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'my-recentf-exclude-p)

(recentf-mode +1)

;; enable window configuration back/forth C-c <-/->
(winner-mode t)

;; windmove enables moving between windows with S - <-/->
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
;; (require 'super-save)
;; ;; add integration with ace-window
;; (add-to-list 'super-save-triggers 'ace-window)
;; (super-save-mode +1)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
        (with-current-buffer buffer (if mode (funcall mode)))))

;; Highlights the current cursor line
(global-hl-line-mode t)
(set-face-background hl-line-face "gray35")

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
;; (require 'rect)
;; (crux-with-region-or-line kill-region)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defcustom my-flyspell t
  "Non-nil values enable Prelude's flyspell support."
  :type 'boolean
  :group 'editor)

(defun my-enable-flyspell ()
  "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
  (when (and my-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(defcustom my-clean-whitespace-on-save t
    "Cleanup whitespace from file before it's saved.
Will only occur if `prelude-whitespace' is also enabled."
    :type 'boolean
    :group 'editor)

(defun my-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
  (when my-clean-whitespace-on-save
    (whitespace-cleanup)))

(defcustom my-whitespace t
  "Non-nil values enable Prelude's whitespace visualization."
  :type 'boolean
  :group 'editor)

(defun my-enable-whitespace ()
  "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
  (when my-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'my-cleanup-maybe nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'my-enable-flyspell)
(add-hook 'text-mode-hook 'my-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; (require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
      bookmark-save-flag 1)

;; projectile is a project management mode
;; (require 'projectile)
;; (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
;; (projectile-mode t)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)
;; (global-set-key (kbd "s-y") 'browse-kill-ring)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(defcustom my-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'editor)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) my-yank-indent-threshold)
      (indent-region beg end nil)))

(defcustom my-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'editor)

(defcustom my-yank-indent-modes '(LaTeX-mode TeX-mode)
    "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
    :type 'list
    :group 'editor)

(defmacro advise-commands (advice-name commands class &rest body)
    "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
    `(progn
       ,@(mapcar (lambda (command)
                   `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                      ,@body))
                 commands)))

(advise-commands "indent" (yank yank-pop) after
                   "If current mode is one of `prelude-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
                   (if (and (not (ad-get-arg 0))
                            (not (member major-mode my-indent-sensitive-modes))
                            (or (derived-mode-p 'prog-mode)
                                (member major-mode my-yank-indent-modes)))
                       (let ((transient-mark-mode nil))
                         (yank-advised-indent-function (region-beginning) (region-end)))))

;; (diminish 'undo-tree-mode)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(setq eshell-directory-name (expand-file-name "eshell" savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" savefile-dir))

;; Compilation from Emacs
(defun my-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)

;; diff-hl
;; (global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
    "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:
  emacsclient filename:linenumber
and file 'filename' will be opened and cursor set on line 'linenumber'"
    (ad-set-arg 0
                (mapcar (lambda (fn)
                          (let ((name (car fn)))
                            (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                                (cons
                                 (match-string 1 name)
                                 (cons (string-to-number (match-string 2 name))
                                       (string-to-number (or (match-string 3 name) ""))))
                              fn))) files)))

;; use settings from .editorconfig file when present
;; (require 'editorconfig)
;; (editorconfig-mode 1)

;; Enable line numbers
;;(linum-mode)  ;; enables only for current buffer
(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode))
  ;;(global-linum-mode t))

;; Add solid line after line number
(setq linum-format "%4d \u2502 ")

;; Show matching parentheses
(show-paren-mode t)

;; Insert matcing parentheses
(electric-pair-mode t)

;; Make auto-indentation globally availabe
(electric-indent-mode t)

;; enable ido mode
;; (setq indo-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode t)

;; Set window title to full file name
;; (setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f"))

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
