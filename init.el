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
   (if (eq system-type 'windows-nt) "USERNAME" "USER")))

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
  :ensure t
  :defer t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; remove warnings about deprecated cl library on load
(setq byte-compile-warnings '(cl-functions))

;; show history of file dependency on cl
;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7) ;; in days
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-delete-old-versions t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe))

;; Which Key, a feature that displays the key bindings following the incomplete command.
;; (use-package which-key
;;   :diminish
;;   :custom
;;   (which-key-separator " ")
;;   (which-key-prefix-prefix "+")
;;   :config
;;   (which-key-mode))

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

;; flycheck on the fly checking code
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

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
  :bind (("S-<Backspace>"   . hungry-delete-backward)
          ("C-c d" . kill-whole-line))
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

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/Users/jm/miniconda3/envs/tf/bin/pandoc"))

;; (concat
;;  "/usr/local/bin/pandoc"
;;  " --from=markdown --to=html"
;;  " --standalone --mathjax --highlight-style=pygments"))

;; LaTeX
(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (declare-function my-latex-mode-hook "init.el")
  :hook ((LaTeX-mode latex-mode) . my-latex-mode-hook)
  :custom
  (TeX-auto-save t)  ; enable parse on load
  (TeX-parse-self t)  ; enable parse on save
  (reftex-plug-into-AUCTeX t)
  (latex-run-command "pdflatex")
  (TeX-source-correlate-start-server t)
  (TeX-source-correlate-method 'synctex)
  (global-visual-line-mode t)
  (TeX-view-program-list
        '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
          ("Skim" "displayline -b -g %n %o %b")
          ("Zathura"
           ("zathura %o"
            (mode-io-correlate
             " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\"")))))
  ;; (TeX-view-program-list '(("Skim" "displayline -b -g %n %o %b")))
  ;; ;; to use pdfview with auctex
  ;; (TeX-view-program-selection '((output-pdf "PDF Viewer")))
  ;; (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  ;; (TeX-view-program-list '(("pdf-tools" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;; (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; :hook (((LaTeX-mode latex-mode) . LaTeX-math-mode)
  ;;        ((LaTeX-mode latex-mode) . flyspell-mode)
  ;;        ((LaTeX-mode latex-mode) . reftex-mode)
  ;;        ((LaTeX-mode latex-mode) . auto-fill-mode)
  ;;        ((LaTeX-mode latex-mode) . visual-line-mode))
  :config
  (setq TeX-outline-extra '(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
                            ("\\\\bibliography\\b" 2))) ; adding own headings to outline minor mode
  ;; Use Skim as viewer, enable source <-> PDF sync
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  ;; (add-hook 'LaTeX-mode-hook (lambda ()
  ;;                              (push
  ;;                               '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
  ;;                                 :help "Run latexmk on file")
  ;;                               TeX-command-list)))
  ;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection '((output-pdf "Skim"))))
  ;; For linux, use Okular or perhaps Zathura.
  (when (eq system-type 'windows-nt)
    (setq TeX-view-program-selection '((output-pdf "Okular"))))

  (defun my-latex-mode-hook ()
    (LaTeX-math-mode)
    (turn-on-flyspell)
    (turn-on-auto-fill)
    (turn-on-visual-line-mode)
    (abbrev-mode)
    (outline-minor-mode)
    ;; (setq TeX-master (my-guess-TeX-master (buffer-file-name)))
    )
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("All" "latexmk -pdf %t" TeX-run-TeX nil
                    (latex-mode doctex-mode)
                    :help "Run latexmk")))
)

;; (defun tex-view ()
;;   (interactive)
;;   (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

(use-package reftex
  :hook ((LaTeX-mode latex-mode) . reftex-mode)
  :config
  (setq-default reftex-enable-partial-scans t
                reftex-save-parse-info t
                reftex-use-multiple-selection-buffers t
                reftex-plug-into-AUCTeX t
                reftex-cite-format 'natbib
                reftex-cite-prompt-optional-args nil
                reftex-cite-cleanup-optional-args t

                reftex-section-levels '(("part" . 0)
                                        ("chapter" . 1)
                                        ("section" . 2)
                                        ("subsection" . 3)
                                        ("frametitle" . 4)
                                        ("subsubsection" . 4)
                                        ("paragraph" . 5)
                                        ("subparagraph" . 6)
                                        ("addchap" . -1)
                                        ("addsec" . -2))

                ;; reftex-plug-into-AUCTeX t
                reftex-extra-bindings t
                reftex-bibfile-ignore-list nil
                reftex-guess-label-type t
                reftex-revisit-to-follow t
                reftex-use-fonts t              ; make colorful toc
                reftex-toc-follow-mode nil      ; don't follow other toc(s)
                reftex-toc-split-windows-orizontally t
                reftex-auto-recenter-toc t
                reftex-enable-partial-scans t
                reftex-save-parse-info t
                reftex-use-multiple-selection-buffers t

                TeX-fold-env-spec-list '(("[comment]" ("comment"))
                                         ("[figure]" ("figure"))
                                         ("[table]" ("table"))
                                         ("[itemize]" ("itemize"))
                                         ("[enumerate]" ("enumerate"))
                                         ("[description]" ("description"))
                                         ("[overpic]" ("overpic"))
                                         ("[tabularx]" ("tabularx"))
                                         ("[code]" ("code"))
                                         ("[shell]" ("shell")))
                )
  )


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

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-begin-commands '(self-insert-command)))

(use-package pyvenv
  :ensure t
  :config
  (defalias 'workon 'pyvenv-workon))
;; :hook ((python-mode . pyvenv-mode))

;; elpy brings dependencies(find-file-in-project, ivy, pyvenv, company,
;;                          highlight-indentation, yasnippet, s)
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (when (load "highlight-indent-guides" t t)
    (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
    (add-hook 'elpy-mode-hook 'highlight-indent-guides-mode))
  (when (package-installed-p '(highlight-indentation))
    (package-delete '(highlight-indentation))))

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
