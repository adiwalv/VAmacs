;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dashboard configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'wid-edit))

;; Dashboard
(when va-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :defines persp-special-last-buffer
    :functions widget-forward winner-undo open-custom-file
    :commands dashboard-insert-startupify-lists
    :preface
    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))))

    (defun exit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (winner-undo))

    (defun dashboard-edit-config ()
      "Open custom config file."
      (interactive)
      (exit-dashboard)
      (open-custom-file))
    :bind (("<f2>" . (lambda ()
                       "Open the *dashboard* buffer and jump to the first widget."
                       (interactive)
                       (if (get-buffer dashboard-buffer-name)
                           (kill-buffer dashboard-buffer-name))
                       (dashboard-insert-startupify-lists)
                       (switch-to-buffer dashboard-buffer-name)
                       (goto-char (point-min))
                       (widget-forward 1)
                       (delete-other-windows)))
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("E" . dashboard-edit-config)
           ("R" . restore-session)
           ("U" . va-update)
           ("q" . exit-dashboard))
    :hook (after-init . dashboard-setup-startup-hook)
    :init (setq inhibit-startup-screen t)
    :config
    (setq dashboard-banner-logo-title "Welcome to VA Emacs")
    (setq dashboard-startup-banner (if va-logo va-logo 'official))
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5)))

    (defun dashboard-insert-buttons (_list-size)
      (insert "\n")
      (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 51) 2))) ?\ ))
      (widget-create 'url-link
                     :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                     :help-echo "Open the VA Emacs Github page"
                     :mouse-face 'highlight
                     va-homepage)
      (insert " ")
      (widget-create 'push-button
                     :help-echo "Restore previous session"
                     :action (lambda (&rest _) (restore-session))
                     :mouse-face 'highlight
                     :button-prefix ""
                     :button-suffix ""
                     (propertize "Restore Session" 'face 'font-lock-keyword-face))
      (insert " ")
      (widget-create 'push-button
                     :help-echo "Edit Personal Configurations"
                     :action (lambda (&rest _) (dashboard-edit-config))
                     :mouse-face 'highlight
                     :button-prefix ""
                     :button-suffix ""
                     (propertize "Edit Config" 'face 'font-lock-keyword-face))
      (insert " ")
      (widget-create 'push-button
                     :help-echo "Update VA Emacs config and packages"
                     :action (lambda (&rest _) (va-update))
                     :mouse-face 'highlight
                     (propertize "Update" 'face 'font-lock-keyword-face))
      (insert "\n")
      (insert "\n")
      (insert (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))

    (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-buttons))
    (add-to-list 'dashboard-items '(buttons))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
