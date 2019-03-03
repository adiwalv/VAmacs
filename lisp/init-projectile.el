;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Projectile configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files:
  ;; ripgrep `rg', the platinum searcher `pt' or the silver searcher `ag'
  (let ((command
         (cond
          ((executable-find "rg")
           (let ((rg-cmd ""))
             (dolist (dir projectile-globally-ignored-directories)
               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
             (concat "rg -0 --files --color=never --hidden" rg-cmd)))
          ((executable-find "pt")
           (if sys/win32p
               (concat "pt /0 /l /nocolor /hidden ."
                       (mapconcat #'identity
                                  (cons "" projectile-globally-ignored-directories)
                                  " /ignore:"))
             (concat "pt -0 -l --nocolor --hidden ."
                     (mapconcat #'identity
                                (cons "" projectile-globally-ignored-directories)
                                " --ignore="))))
          ((executable-find "ag")
           (concat "ag -0 -l --nocolor --hidden"
                   (mapconcat #'identity
                              (cons "" projectile-globally-ignored-directories)
                              " --ignore-dir="))))))
    (setq projectile-generic-command command))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "rg") (executable-find "pt") (executable-find "ag"))
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val))

  ;; Rails project
  (use-package projectile-rails
    :diminish projectile-rails-mode
    :hook (projectile-mode . projectile-rails-global-mode)))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
