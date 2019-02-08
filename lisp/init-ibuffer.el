;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix "Project: ")
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
config
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file (file &optional wildcards)
      "Like `find-file', but default to the directory of the buffer at point."
      (interactiveconfig
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (buffer-local-value 'default-directory buf)
                                    default-directory))))
         (counsel-find-file))))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file)))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
