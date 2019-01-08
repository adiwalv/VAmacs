;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(defgroup va nil
  "VAmacs customizations."
  :group 'convenience)

(defcustom va-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set VAmacs logo. nil means official logo."
  :type 'string)

(defcustom va-full-name "Vikas Adiwal"
  "Set user full name."
  :type 'string)

(defcustom va-mail-address "adiwalv@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom va-proxy "127.0.0.1:1087"
  "Set network proxy."
  :type 'string)

(defcustom va-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)))

(defcustom va-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Dracula theme" dracula)
          (const :tag "Classic theme" classic)
          (const :tag "Doom theme" doom)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Daylight theme" daylight)
          symbol))

(defcustom va-cnfonts nil
  "Use cnfonts or not."
  :type 'boolean)

(defcustom va-dashboard t
  "Use dashboard at startup or not.

If Non-nil, use dashboard, otherwise will restore previous session."
  :type 'boolean)

(defcustom va-lsp 'lsp-mode
  "Set language server."
  :type '(choice
          (const :tag "LSP Mode" 'lsp-mode)
          (const :tag "eglot" 'eglot)
          nil))

(defcustom va-ivy-icon (and (not sys/win32p) (display-graphic-p))
  "Display icons in `ivy' or not."
  :type 'boolean)

(defcustom va-pretty-magit t
  "Prettify `magit' or not."
  :type 'boolean)

(defcustom va-company-enable-yas nil
  "Enable yasnippet for company backends or not."
  :type 'boolean)

(defcustom va-benchmark nil
  "Enable the init benchmark or not."
  :type 'boolean)

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
