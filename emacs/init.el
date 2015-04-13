

;;; packages
(setq required-packages
      '(use-package cl multi-term discover org-page hydra solarized-theme))

;; init packages
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; install packages
(mapc (lambda (p)
	(unless (package-installed-p p)
	  (package-install p)))
      required-packages)

;; setup use-package
(eval-when-compile
  (require 'use-package))


;; prompt before closing emacs with C-x C-c
(setq confirm-kill-emacs 'yes-or-no-p)


;;; theme
;; solarized
(load-theme 'solarized-light t)
(set-face-attribute 'default nil :height 100)



;;; remote
(defvar my-hosts (list "admin" "asp5" "asp6" "asp7" "asp8" "wlp1" "wlp2" "asd5" "asd6" "wlt1"))
(defvar my-users (list "a-iclark" "weblogic" "root"))

(defun remote-find-file (host user)
  "Start find-file remotely on HOST with USER"
  (interactive)
  (let* ((tramp-hop  (if (string-match-p "^a-" user) "" (concat "|sudo:" user "@" host)))
	 (tramp-host (concat "/ssh:" host tramp-hop ":")))
    (find-file
     (ido-read-file-name
      (concat user "@" host ": ")
      tramp-host))))

(defun my-remote-tramp-prompt ()
  "Prompt for host and user, begin find-file remotely"
  (interactive)
  (let* ((host (ido-completing-read "Host: " my-hosts))
	 (user (ido-completing-read "User: " my-users)))
    (remote-find-file host user)))



;;; utils
(defun slurp-file (filename)
  "Reads FILENAME as a string"
  (let ((real-filename (expand-file-name filename)))
    (if (file-exists-p real-filename)
        (with-temp-buffer
          (insert-file-contents real-filename)
          (buffer-string))
      "")))


(defun slurp-file-lines (filename)
  "Reads FILENAME as a list of strings"
  (split-string (slurp-file filename) "\n"))


(defun split-shell-assignment (lines)
  "Takes a list of strings and splits on the first ="
  (mapcar (lambda (parts)
	    (list (car parts)
		  (mapconcat 'identity (cdr parts) "=")))
	  (mapcar (lambda (s) (split-string s "="))
		  lines)))


(defun mapapply (func lst)
  "Apply func to each element in lst"
  (when lst
    (cons (apply func (car lst))
	  (mapapply func (cdr lst)))))


(defun source-file (filename)
  "Applies environment variables from filename"
  (let ((lines (delq "" (slurp-file-lines filename))))
    (mapapply 'setenv (split-shell-assignment lines))))


(source-file "~/.gpg-agent-info")	; pull in gpg agent environment vars for tramp


;;; misc options
(show-paren-mode 1)			; show matching parenthesis
(line-number-mode 1)			; show line number in status bar
(global-linum-mode t)			; show line numbers 

;; save backup files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;;; keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'my-remote-tramp-prompt)


;;; ido-mode
(require 'ido)
(ido-mode t)



;; set frequently used files
(mapc
 (lambda (r) (set-register (car r) (cons 'file (cdr r))))
 '((?i . "~/.emacs.d/init.el")
   (?o . "~/org/ewu.org")))


;; blog
(use-package org-page
  :init
  (setq op/repository-directory "~/src/github.io")
  (setq op/site-domain "http://turbana.github.io")
  ;(setq op/personal-avatar "https://avatars0.githubusercontent.com/u/75674?v=3&s=460")
  ;; for commenting; disabled for now
  ;;(setq op/personal-disqus-shortname "your_disqus_shortname")

  ;; analytics set up at ~/.emacs.secrets file
  ;;(setq op/personal-google-analytics-id "UA-NNNNNNNN-N")

  (setq op/personal-github-link "https://github.com/turbana")

  (setq op/site-main-title "Ian's blog")
  (setq op/site-sub-title "My tag line")

  ;; set up my own theme since a sans option does not exist
  ;(setq op/theme-root-directory "~/src/org-page/heikkil.github.io/themes")
  ;(setq op/theme 'sans)  ; mdo is the default

  (global-set-key
   (kbd "C-c b")
   (defhydra hydra-blog (:color blue :hint nil)
     "
blog  _n_: new post                        _l_: publish last commit
      _r_: reset & publish all             _p_: publish interactively
      _t_: reset & publish to /tmp/blog    _e_: new-repository"
     ("n" op/new-post)
     ("r" (progn
	    (setq op/item-cache nil)
	    (op/do-publication t nil nil t t)))
     ("t" (progn
	    (setq op/item-cache nil)
	    (op/do-publication t nil "/tmp/blog" nil nil)))
     ("l" (op/do-publication nil nil nil t t))
     ("p" op/do-publication)
     ("e" op/new-repository)))
  )


(defun blog-new-post ()
  (interactive)
  (let* ((title (read-string "Title: "))
	 (title-enc (encode-string-to-url title))
	 (uri (concat "/blog/%y/%m/%d/" title-enc))
	 (dir (concat (file-name-as-directory op/repository-directory)
		      (file-name-as-directory "blog")))
	 (path (concat dir title-enc ".org")))
    (op/git-change-branch op/repository-directory op/repository-org-branch)
    (if (file-exists-p path)
	(error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (op/insert-options-template title uri "KEYWORDS" "TAGS" "DESCRIPTION")
    (save-buffer)))


;(defun blog-publish ()


;; org-mode
(use-package org
 :init
 (progn
  (setq org-clock-idle-time nil)
  (setq org-log-done 'time)
  (setq org-clock-continuously t)
  (setq org-clock-persist t)
  (setq org-clock-in-switch-to-state "WORKING")
  (setq org-clock-out-switch-to-state "TODO")
  (setq org-clock-in-resume t)
  (setq org-clock-report-include-clocking-task t)
  (global-set-key (kbd "C-c o l") 'org-store-link)
  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o b") 'org-iswitchb)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WORKING(w)" "|" "DONE(d!)")
	  (sequence "|" "WAITING(a@/!)" "HOLD(h@/!)" "PERSISTANT(p)")))
  )

 :config
 (progn
  (org-clock-persistence-insinuate)
  (setq org-log-into-drawer "LOGBOOK"=)
  (setq org-clock-into-drawer 1)))
    




  
;;; shell intergration
(use-package multi-term
  :init
  (setq multi-term-program "/bin/bash"))


;;; tramp setup
(setq tramp-default-method "ssh")
;;(setq tramp-default-user 'nil)
;; 
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(keychain-gpg-file "/home/iclark/.gpg-agent-info")
 '(keychain-ssh-file "/home/iclark/.gpg-agent-info")
 '(org-agenda-files (quote ("~/org/ewu.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
