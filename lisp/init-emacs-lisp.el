;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Lisp configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; Semantic code search for emacs lisp
(use-package elisp-refs)

;; A better *Help* buffer
(use-package helpful
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point))
  :config
  (with-eval-after-load 'ivy
    (dolist (cmd '(helpful-callable
                   helpful-variable
                   helpful-function
                   helpful-macro
                   helpful-command))
      (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist))))

(provide 'init-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
