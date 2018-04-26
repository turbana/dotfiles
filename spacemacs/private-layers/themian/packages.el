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

(defun themian/init-themian-theme ()
  (use-package themian-theme
    :init
    (progn
      (global-set-key (kbd "<f5>")
                      '(lambda ()
                         (interactive)
                         (load-theme 'themian t))))))

;;; packages.el ends here
