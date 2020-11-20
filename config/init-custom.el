;;; init-custom.el --- Package initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
     ("Skim" "displayline -b -g %n %o %b")
     ("Zathura"
      ("zathura %o"
       (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\"")))))
 '(auto-package-update-delete-old-versions t t)
 '(auto-package-update-hide-results t t)
 '(global-visual-line-mode t)
 '(latex-run-command "pdflatex")
 '(lsp-prefer-flymake nil)
 '(package-selected-packages
   '(ivy-bibtex company-lsp lsp-ui fill-column-indicator zenburn-theme exec-path-from-shell highlight-indent-guides pyvenv projectile magit expand-region multiple-cursors hungry-delete py-autopep8 flycheck company-jedi company try hl-todo f diminish use-package))
 '(reftex-plug-into-AUCTeX t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init-custom)
;;; init-custom.el ends here
