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

(defconst themian-packages
  '((themian-theme :location local)))

(defun themian/reload-theme ()
  (interactive)
  (load-theme 'themian t))

(defun themian/init-themian-theme ()
  ;; (message "inside init")
  (use-package themian-theme)
  (global-set-key (kbd "<f5>") 'themian/reload-theme)
  )

;;; packages.el ends here
