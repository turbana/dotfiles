(defvar ian-packages
  '(org))

(defvar ian-excluded-packages '())


(defun ian/init-org ()
  (use-package org
    :init
    ;;; basic setup
    (setq org-directory "~/org")
    (setq org-agenda-files '("~/org" "~/org/work" "~/org/home"))
    (setq org-default-notes-file "~/org/refile.org")
    (setq org-startup-folded t)
    (setq org-archive-location "%s_archive::")


    ;;; appearance
    (setq org-hide-leading-stars t)
    (setq org-odd-levels-only nil)
    (setq org-completion-use-ido t)
    (setq org-return-follows-link t)
    (setq org-blank-before-new-entry nil)


    ;;; todo keywords
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "WAITING(a@/!)" "HOLD(h@/!)" "|"
                      "CANCELLED(c@/!)" "PHONE" "MEETING" "HABIT(a)")))
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)


    ;;; tags
    (setq org-tag-alist
          '(("WORK" . ?w)
            ("HOME" . ?h)))
    (setq org-tags-column -120)


    ;;; logging
    (setq org-log-done 'time)
    (setq org-log-into-drawer "LOGBOOK"=)


    ;;; capture
    (setq org-reverse-note-order nil)
    (setq org-capture-templates
          '(("t" "todo" entry (file org-default-notes-file)
             "* TODO %?\n%U\n"
             :clock-in t :clock-resume t)
            ("r" "respond" entry (file org-default-notes-file)
             "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
             :clock-in t :clock-resume t :immediate-finish t)
            ("n" "note" entry (file org-default-notes-file)
             "* %? :NOTE:\n%U\n"
             :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* MEETING with %? :MEETING:\n%U"
             :clock-in t :clock-resume t)
            ("p" "Phone call" entry (file org-default-notes-file)
             "* PHONE %? :PHONE:\n%U"
             :clock-in t :clock-resume t)
            ("h" "Habit" entry (file org-default-notes-file)
             "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
    (setq org-refile-targets
          '((nil :maxlevel . 6)
            (org-agenda-files :maxlevel . 6)))
    (setq org-refile-use-outline-path t)


    ;;; agenda
    ;; TODO


    ;;; clocking
    ;; resolve open clocks for any amount of time
    (setq org-clock-idle-time nil)
    ;; start new clocking immediatly after clocking out
    (setq org-clock-continuously t)
    ;; save current clock when emacs exits
    (setq org-clock-persist t)
    ;; change to NEXT state when starting a clock
    (setq org-clock-in-switch-to-state "NEXT")
    ;; change to TODO state when leaving a clock
    (setq org-clock-out-switch-to-state "TODO")
    ;; continue an open clock when checking into task
    (setq org-clock-in-resume t)
    ;; include current clock in reports
    (setq org-clock-report-include-clocking-task t)
    ;; save all clocks in the LOGBOOK drawer
    (setq org-clock-into-drawer "LOGBOOK")
    ;; show current clock in the modeline
    (setq spacemacs-mode-line-org-clock-current-taskp t)
    ;; only show today's clock in the modeline
    (setq org-clock-mode-line-total 'today)


    :config
    ;; setup persistant clocks
    (org-clock-persistence-insinuate)
    ;; begin captures in insert mode
    (add-hook 'org-capture-mode-hook 'evil-insert-state)
    ))
