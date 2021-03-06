;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Golang configurations.
;;

;;; Code:
;;
;; Go packages:
;; go get -u github.com/mdempsky/gocode # github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/gotype
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct

(eval-when-compile
  (require 'init-custom))

;; Golang
(use-package go-mode
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-rename)
  (use-package golint)
  (use-package govet)

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c t" . go-tag-add)
                ("C-c T" . go-tag-remove))
    :config (setq go-tag-args (list "-transform" "camelcase")))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run)))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-t" . go-gen-test-dwim)))

  ;; LSP provides the functionalities.
  ;; NOTE: `go-langserver' doesn't support Windows so far.
  (unless centaur-lsp
    ;; `goimports' or `gofmt'
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)

    ;; Go add-ons for Projectile
    ;; Run: M-x `go-projectile-install-tools'
    (with-eval-after-load 'projectile
      (use-package go-projectile
        :commands (go-projectile-mode go-projectile-switch-project)
        :hook ((go-mode . go-projectile-mode)
               (projectile-after-switch-project . go-projectile-switch-project))))

    (use-package go-eldoc
      :hook (go-mode . go-eldoc-setup))

    (use-package go-guru
      :bind (:map go-mode-map
                  ;; ([remap xref-find-definitions] . go-guru-definition)
                  ([remap xref-find-references] . go-guru-referrers)))

    (with-eval-after-load 'company
      (use-package company-go
        :defines company-backends
        :functions company-backend-with-yas
        :init (cl-pushnew (company-backend-with-yas 'company-go) company-backends)))))

;; Local Golang playground for short snippes
(use-package go-playground
  :diminish go-playground-mode
  :commands go-playground-mode)

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
