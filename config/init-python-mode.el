;;; package --- Summary

;;; Commentary:

;;; Code:
;; python-mode
(require 'python)
;; (python-mode)

;; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;; '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; (setq py-force-py-shell-name-p t)

(setq python-shell-interpreter "python" python-shell-interpreter-args "-i")

;; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p nil)
;; (setq py-switch-buffers-on-execute-p nil)
;; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
;; (setq py-smart-indentation t)

;; loading pymacs and ropemacs
;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
;;(setq ropemacs-enable-shortcuts nil)
;;(setq ropemacs-local-prefix "C-c C-p")

(set-variable 'python-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)

(provide 'init-python-mode)
;;; init-python-mode.el ends here
