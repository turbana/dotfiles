(defvar ian-packages
  '(org noflet))

(defvar ian-excluded-packages
  '(tern exec-path-from-shell))


(defun ian/init-noflet ()
  (use-package noflet))


(defun ian/init-mu4e ()
  (use-package mu4e
    :init
    (setq mu4e-maildir "~/.mail/eastern")
    (setq mu4e-drafts-folter "drafts")
    (setq mu4e-sent-folder "sent")
    (setq mu4e-trash-folder "junk")
    ))


(defun ian/init-org ()
  (use-package org
    :init
    ;;; basic setup
    (setq org-directory "~/org")
    (setq org-agenda-files '("~/org" "~/org/work" "~/org/home"))
    (setq org-default-notes-file "~/org/refile.org")
    (setq org-default-jobs-file "~/org/home/jobs.org")
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
            ("HOME" . ?h)
            ("488"  . ?s)))
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
             "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
            ("R" "Recipe" entry (file org-default-notes-file)
             "* %^{name} :uncooked::\n:PROPERTIES:\n:SOURCE: %^{source}\n:IMAGE: %^{image}\n:SERVING: %^{serving}\n:TYPE: %^{type}\n:END:\n** Ingredients\n   | %? | |\n** Procedure\n   1.\n** Log")
            ("A" "Job Application" entry (file org-default-jobs-file)
             "* %^{company} - %^{title}\n  %U\n  [[%^{submission}][Submission]]\n  %?\n** Description\n   %^{description}\n** Contact Info\n** Log\n")
            ))
    (setq org-refile-targets
          '((nil :maxlevel . 6)
            (org-agenda-files :maxlevel . 6)))
    (setq org-refile-use-outline-path t)


    ;;; agenda
    ;; don't show completed DEADLINE and SCHEDULED in agenda
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    ;; first day in agenda should be today
    (setq org-agenda-start-on-weekday nil)
    ;; compact agenda views
    ;(setq org-agenda-compact-blocks t)
    ;; show daily agenda by default
    (setq org-agenda-span 'day)
    ;; custom agenda views
    (setq org-agenda-custom-commands
          '((" " "Agenda"
             ((agenda "" nil)
              (tags "REFILE"
                    ((org-agenda-overriding-header "Refile tasks:")
                     (org-tags-match-list-sublevels nil)))
              (tags "-REFILE/"
                      ((org-agenda-overriding-header "Archive tasks:")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil)))


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


    ;;; archiving
    ;; don't modify the task state when archiving
    (setq org-archive-mark-done nil)


    ;;; exporting
    (setq org-publish-project-alist
          '(("recipes"
             :base-directory "~/org/home/food/"
             :base-extension "org"
             :publishing-directory "~/food/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :auto-preamble t
             )
            ))


    :config
    ;; setup persistant clocks
    (org-clock-persistence-insinuate)
    ;; begin captures in insert mode
    (add-hook 'org-capture-mode-hook 'evil-insert-state)
    (add-hook 'org-export-before-parsing-hook 'ic/org-export-filter-recipes)
    ))
