;; init-elfeed.el --- Initialize elfeed.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; A RSS feed reader.
;;

;;; Code:

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory))
  (setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast")))

(provide 'init-elfeed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elfeed.el ends here
