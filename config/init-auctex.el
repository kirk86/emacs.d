;;; package --- Summary
;;  AucTex

;;; Commentary:
;; ##### AucTex settings

;;; Code:
;; ##### AucTex settings
;; (require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;(setq TeX-save-query nil)
(setq-default TeX-master nil)

;; ##### turn on LaTeX Math mode by default
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; ##### turn on auto fill in LaTeX
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; ##### setup for master file when working on projects with multiple .tex files
(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (setq TeX-master (guess-TeX-master (buffer-file-name)))))

;; ##### turn on Visual Line Mode by default
(global-visual-line-mode t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; ##### turn on on-the-fly spell checking
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; ##### enable RefTeX in AUCTeX (LaTeX-mode)
(add-hook 'latex-mode-hook 'turn-on-reftex) ;; ##### with Emacs latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;; ##### with AUCTeX LaTeX mode
(setq reftex-plug-into-auctex t)
;; (global-auto-revert-mode t)

;; ##### Enable preview-latex the default coming with AucTex
;; After installation, the package may need to be activated
;; (and remember to activate AUCTeX too). In XEmacs, and in any prepackaged
;; versions worth their salt, activation should be automatic upon installation.
;; If this seems not the case, complain to your installation provider and read on.
;; You might salvage this by adding the following activation line to your own
;; .emacs file or to a site-wide initialization file:
;;(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

;; There might still be missing an autoload form in which case you would have to add
;;(autoload 'LaTeX-preview-setup "preview")

;; And in case the directory to which the Elisp files have been installed is
;; not known to Emacs, a line like below needs to be added as well.
;;(add-to-list 'load-path "/whatever/directory")

;; ##### latex-pretty-symbols for latex preview
;; (require 'latex-pretty-symbols)

;; ##### Try this
(setq latex-run-command "pdflatex")
;; ##### If this doesn't work
;;(setq-default TeX-PDF-mode t) ;; ##### default pdflatex mode
;;(require 'tex)
;;(TeX-global-PDF-mode t)

;; ##### Enable synctex correlation. From Okular just press
;; ##### Shift + Left click to go to the good line.
(setq TeX-source-correlate-method 'synctex)
;; ##### Enable synctex generation. Even though the command shows
;; ##### as "latex" pdflatex is actually called
(custom-set-variables '(LaTeX-command "latex -synctex=1") )

;; auctex-preview: keep old preview visible when editing
;; (defadvice preview-inactive-string (around preview-show-old nil activate)
;;   "Show old preview when editing source code."
;;   (when (overlay-get ov 'preview-state)
;;     (let ((preview-icon (or (car-safe (overlay-get ov 'preview-image)) preview-icon)))
;;       (overlay-put ov 'preview-old-image preview-icon)
;;       ad-do-it
;;       )))

;; (defadvice preview-disabled-string (around preview-show-old nil activate)
;;   "Show old preview when editing source code."
;;   (when (overlay-get ov 'preview-state)
;;     (let ((preview-icon (or (overlay-get ov 'preview-old-image) preview-icon)))
;;       ad-do-it
;;             )))

;; ;; auctex-preview: Scale preview images with respect to the buffer-local face size
;; (require 'face-remap)
;; (defadvice preview-inherited-face-attribute (after preview-inherit-local-face nil activate)
;;   "Scale preview images with respect to buffer-local face"
;;   (when (eq attribute :height)
;;     (message "advice preview-inherit-face-attribute")
;;     (let ((scale-amount text-scale-mode-amount)
;;           (scale-factor text-scale-mode-step))
;;       (when (< scale-amount 0)
;;         (setq scale-amount (- scale-amount))
;;         (setq scale-factor (/ 1.0 scale-factor)))
;;       (while (> scale-amount 0)
;;         (setq ad-return-value (* scale-factor ad-return-value))
;;         (setq scale-amount (1- scale-amount)))
;;             )))

;; adding own headings to auctex outline minor mode
(setq TeX-outline-extra
      '(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
        ("\\\\bibliography\\b" 2)))

(provide 'init-auctex)
;;; init-auctex.el ends here