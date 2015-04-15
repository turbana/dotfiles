(defvar ian-packages
  '(org)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ian-excluded-packages '()
  "List of packages to exclude.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ian/init-org ()
  (use-package org
    :init
    (progn
      ;;; clocking
      (setq org-clock-idle-time nil)
      (setq org-log-done 'time)
      (setq org-clock-continuously t)
      (setq org-clock-persist t)
      (setq org-clock-in-switch-to-state "NEXT")
      (setq org-clock-out-switch-to-state "TODO")
      (setq org-clock-in-resume t)
      (setq org-clock-report-include-clocking-task t)
      ;; remove empty drawers
      ;(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

      ;;; todo states
      ;; use fast todo selections
      (setq org-use-fast-todo-selection t)
      ;; change states with S-left/right, skipping processing
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)
      ;; todo keywords
      (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(a@/!)" "HOLD(h@/!)" "|"
                        "CANCELLED(c@/!)" "PHONE" "MEETING" "HABIT(a)")))
      ;; todo tags
      (setq org-todo-state-tags-triggers
            '(("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

      ;;; capture
      ;; set default target
      (setq org-directory "~/org")
      (setq org-default-notes-file "~/org/refile.org")
      ;; capture templates
      (setq org-capture-templates
            '(("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n"
               :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
               :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n"
               :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree org-default-notes-file)
               "* %?\n%U\n"
               :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-default-notes-file)
               "* TODO Review %c\n%U\n"
               :immediate-finish t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%U"
               :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U"
               :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-default-notes-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

      ;;; agenda
      ;; set org agenda directories
      (setq org-agenda-files '("~/org"
                               "~/org/home"
                               "~/org/work"))
      ;; refile targets include current file and any agenda file
      (setq org-refile-targets '((nil :maxlevel . 6)
                                 (org-agenda-files :maxlevel . 6)))
      ;; use full paths for refile targets
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps nil)
      ;; allow refile to create parent tasks
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      ;; Do not dim blocked tasks
      (setq org-agenda-dim-blocked-tasks nil)

      ;; Compact the block agenda view
      (setq org-agenda-compact-blocks t)

      ;; Custom agenda command definitions
      (setq org-agenda-custom-commands
            '(("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "* REFILE TASKS *")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including waiting and scheduled tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-cancelled+waiting|hold/!"
                           ((org-agenda-overriding-header (concat "waiting and postponed tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including waiting and scheduled tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-refile/"
                      ((org-agenda-overriding-header "tasks to archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil)))

      ;; ido setup (from norang.ca)
      ;; Use IDO for both buffer and file completion and ido-everywhere to t
      (setq org-completion-use-ido t)
      (setq ido-everywhere t)
      (setq ido-max-directory-size 100000)
      (ido-mode (quote both))
      ;; Use the current window when visiting files and buffers with ido
      (setq ido-default-file-method 'selected-window)
      (setq ido-default-buffer-method 'selected-window)
      ;; Use the current window for indirect buffer display
      (setq org-indirect-buffer-display 'current-window)

      ;; TODO default task
      (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")
      )

    :config
    (progn
      (org-clock-persistence-insinuate)
      (setq org-log-into-drawer "LOGBOOK"=)
      (setq org-clock-into-drawer 1)
      (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
      ))
  )
