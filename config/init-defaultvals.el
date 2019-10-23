;;; package --- Summary

;;; Commentary:

;;; Code:

(defcustom my-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'compilation)

(defcustom my-flyspell t
  "Non-nil values enable Prelude's flyspell support."
  :type 'boolean
  :group 'editor)

(defcustom my-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `prelude-whitespace' is also enabled."
  :type 'boolean
  :group 'editor)

(defcustom my-whitespace t
  "Non-nil values enable Prelude's whitespace visualization."
  :type 'boolean
  :group 'editor)

(defcustom my-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'editor)

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


(provide 'init-groups)
;;; init-groups.el ends here
