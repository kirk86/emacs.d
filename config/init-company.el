;;; package --- Summary
;;; Commentary:

;;; Code:
(defun my/python-mode-hook ()
  "Python hook for company."
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)

(provide 'init-company)
;;; init-company.el ends here
