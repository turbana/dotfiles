;;; packages.el --- themian layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ian Clark <ian@cyclone.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst ian-packages
  '((themian-theme :location local)))

(defun ian/init-themian-theme ()
  (use-package themian-theme
    :init
    (progn
      (global-set-key (kbd "<f5>")
                      '(lambda ()
                         (interactive)
                         ;; (message "foo")
                         (disable-theme 'themian-dark)
                         (load-file "themian-theme.el")
                         (load-file "themian-dark-theme.el")
                         (load-theme 'themian-dark t)))
      (global-set-key (kbd "<f6>")
                      '(lambda ()
                         (interactive)
                         (load-file "themian-theme.el")
                         (load-file "themian-light-theme.el")
                         (load-theme 'themian-light t)))
                      )))

;;; packages.el ends here
