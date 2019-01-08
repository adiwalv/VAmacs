;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Language Server Protocol configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(pcase va-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish lsp-mode
     :hook (prog-mode . lsp)
     :init
     ;; Support LSP in org babel
     ;; https://github.com/emacs-lsp/lsp-mode/issues/377
     (cl-defmacro lsp-org-babel-enbale (lang)
       "Support LANG in org source code block."
       ;; (cl-check-type lang symbolp)
       (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
              (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
         `(progn
            (defun ,intern-pre (info)
              (let ((lsp-file (or (->> info caddr (alist-get :file))
                                  buffer-file-name)))
                (setq-local buffer-file-name lsp-file)
                (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
                (lsp)))
            (if (fboundp ',edit-pre)
                (advice-add ',edit-pre :after ',intern-pre)
              (progn
                (defun ,edit-pre (info)
                  (,intern-pre info))
                (put ',edit-pre 'function-documentation
                     (format "Prepare local buffer environment for org source block (%s)."
                             (upcase ,lang))))))))

     (defvar org-babel-lang-list
       '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
     (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
     (dolist (lang org-babel-lang-list)
       (eval `(lsp-org-babel-enbale ,lang)))

     (setq lsp-auto-guess-root t)       ; Detect project root
     :config (require 'lsp-clients))

   (use-package lsp-ui
     :bind (:map lsp-ui-mode-map
                 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                 ([remap xref-find-references] . lsp-ui-peek-find-references)
                 ("C-c u" . lsp-ui-imenu)))

   (use-package company-lsp)

   ;; C/C++/Objective-C support
   (use-package ccls
     :defines projectile-project-root-files-top-down-recurring
     :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                      (require 'ccls)
                                                      (lsp)))
     :config
     (with-eval-after-load 'projectile
       (setq projectile-project-root-files-top-down-recurring
             (append '("compile_commands.json"
                       ".ccls")
                     projectile-project-root-files-top-down-recurring))))

   ;; Java support
   (use-package lsp-java
     :hook (java-mode . (lambda ()
                          (require 'lsp-java)
                          (lsp))))
   ))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
