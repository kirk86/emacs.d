;;; init-custom.el --- Package initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-auto-save t t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-method (quote synctex) t)
 '(auto-package-update-delete-old-versions t t)
 '(auto-package-update-hide-results t t)
 '(global-visual-line-mode t)
 '(latex-run-command "pdflatex")
 '(lsp-prefer-flymake nil)
 '(package-selected-packages
   (quote
    (ivy-bibtex company-lsp lsp-ui fill-column-indicator zenburn-theme exec-path-from-shell highlight-indent-guides pyvenv projectile magit expand-region multiple-cursors hungry-delete py-autopep8 flycheck company-jedi company try hl-todo f diminish use-package)))
 '(reftex-plug-into-AUCTeX t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init-custom)
;;; init-custom.el ends here
