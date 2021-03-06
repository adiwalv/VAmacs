;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq va-logo nil)                        ; Logo file or nil (official logo)
;; (setq va-full-name "user name")           ; User full name
;; (setq va-mail-address "user@email.com")   ; Email address
;; (setq va-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq va-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china netease or tuna
;; (setq va-theme 'classic)                  ; Color theme: default, classic, doom, dark, light or daylight
;; (setq va-cnfonts t)                       ; Use cnfonts not: t or nil
;; (setq va-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq va-lsp nil)                         ; Set LSP client: lsp-mode, eglot or nil
;; (setq va-ivy-icon nil)                    ; Display icons in ivy or not: t or nil
;; (setq va-pretty-magit nil)                ; Prettify magit or not: t or nil
;; (setq va-company-enable-yas t)            ; Enable yasnippet for company or not: t or nil
;; (setq va-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (and (not va-cnfonts) (display-graphic-p))
  ;; Set a default font
  (cond
   ((member "Source Code Pro" (font-family-list))
    (set-face-attribute 'default nil :font "Source Code Pro"))
   ((member "Menlo" (font-family-list))
    (set-face-attribute 'default nil :font "Menlo"))
   ((member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco"))
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
   ((member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas")))

  (cond
   (sys/mac-x-p
    (set-face-attribute 'default nil :height 130))
   (sys/win32p
    (set-face-attribute 'default nil :height 110)))

  ;; Specify font for all unicode characters
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend))

  ;; Specify font for chinese characters
  (cond
   ((member "WenQuanYi Micro Hei" (font-family-list))
    (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
   ((member "Microsoft Yahei" (font-family-list))
    (set-fontset-font t '(#x4e00 . #x9fff) "Microsoft Yahei")))
  )

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
