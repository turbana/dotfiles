;;; init.el --- Ian Clark's Initialization File
;;
;; Copyright (c) 2020 Ian Clark
;;
;; Author: Ian Clark <turbana@gmail.com>
;; URL: https://github.com/turbana/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; setup emac's package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


;;; setup use-package
(package-install 'use-package t)
(eval-when-compile
  (require 'use-package))
;; (setq use-package-always-ensure t)
;; (require 'diminish)
;; (require 'bind-key)


;;; setup org
(package-install 'org t)
(require 'org)


;;; tangle/load the real config
(load-file
 (let* ((org-file (expand-file-name "~/.etc/emacs/emacs.org"))
        (el-file (concat (substring org-file 0 -4) ".el")))
   (if (file-newer-than-file-p org-file el-file)
       (car (org-babel-tangle-file org-file el-file))
     el-file)))
