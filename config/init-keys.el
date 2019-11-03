;;; init-keys.el --- keybindings                     -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2019  jm

;; Author: jm <jm@insight026.local>
;; Keywords: Lisp

;;; Code:

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
;; (define-key lisp-mode-shared-map (kbd "M-(") (prelude-wrap-with "("))
;; FIXME: Pick terminal-friendly binding.
;;(define-key lisp-mode-shared-map (kbd "M-[") (prelude-wrap-with "["))
;; (define-key lisp-mode-shared-map (kbd "M-\"") (prelude-wrap-with "\""))

;; (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)


(provide 'init-keys)
;;; init-keys.el ends here
