;;; package --- Summary
;;  AucTex

;;; Commentary:
;; ##### AucTex settings

;;; Code:
;; ##### AucTex settings
;; (require 'tex-site)

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

;; ##### If this doesn't work
;;(setq-default TeX-PDF-mode t) ;; ##### default pdflatex mode
;;(require 'tex)
;;(TeX-global-PDF-mode t)

;; ##### Enable synctex correlation. From Okular just press
;; ##### Shift + Left click to go to the good line.

;; ##### Enable synctex generation. Even though the command shows
;; ##### as "latex" pdflatex is actually called
;; (custom-set-variables '(LaTeX-command "latex -synctex=1") )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guess tex master file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el --- Guess LaTeX Master File
;;
;; Filename: init-latex.el
;; Description: Guess LaTeX Master File
;; Author: Unknown & Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Dec 12 14:12:47 2011 (-0600)
;; Version:  0.02
;; Last-Updated: Mon Dec 12 15:31:35 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 56
;; URL: https://github.com/mlf176f2/guess-tex-master.el
;; Keywords: AucTeX TeX-master
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 12-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Mon Dec 12 15:17:17 2011 (-0600) #55 (Matthew L. Fidler)
;;    Bugfix
;; 12-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Mon Dec 12 14:55:15 2011 (-0600) #31 (Matthew L. Fidler)
;;    Initial release
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defgroup guess-TeX-master nil
  "Guess TeX Master from either the open files or files in the
current directory.  Then optionally set the master file via a
local variable."
  :group 'AUCTeX)

(defcustom guess-TeX-master-includes '("input" "include" "makerubric")
  "List of known LaTex includes"
  :type '(repeat (string :tag "Include Tag"))
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-buffers t
  "Guess LaTeX Master from current buffer?"
  :type 'boolean
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-files t
  "After failing to guess the TeX master from buffers, guess LaTeX
master from current files? (Requires egrep)"
  :type 'boolean
  :group 'guess-TeX-master)

(defun guess-TeX-master-from-files (filename)
  "Guess TeX master from egrep list of files"
  (let (val master)
    ;; Unimplemented.
  (symbol-value 'master)))

(defun guess-TeX-master-from-buffer (filename)
  "Guesses TeX master from open .tex buffers"
  (let (candidate)
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward (concat "\\\\"
                                                   (regexp-opt guess-TeX-master-includes t)
                                                   "{" filename "\\([.]tex\\)?}") nil t)
                    (setq candidate file))))))))
    (symbol-value 'candidate)))

;;;###autoload
(defun guess-TeX-master ()
  "Guess the master file for FILENAME"
  (let* ((candidate nil)
        (filename (buffer-file-name))
        (filename (file-name-sans-extension (file-name-nondirectory filename))))

    (when guess-TeX-master-from-buffers
      (setq candidate (guess-TeX-master-from-buffer filename)))
    (setq candidate (guess-TeX-master-from-files filename))
    (when candidate
      (message "TeX master document: %s" (file-name-nondirectory candidate))
      (unless TeX-master
        (set (make-local-variable 'TeX-master) candidate)))))

;;;###autoload
(add-hook 'LaTeX-mode-hook 'guess-TeX-master)
;;;###autoload
(add-hook 'TeX-mode-hook 'guess-TeX-master)

(provide 'init-latex)
;;; init-latex.el ends here
