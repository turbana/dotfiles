;;; themian-theme.el --- My color theme

;;; Commentary:

;;; Code:


(defmacro themian-with-color-variables (variant &rest body)
  (declare (indent 0))
  `(let* ((class '((class color) (min-colors 89)))
          (dark (eq variant 'dark))
          (unknown  "#9933ff")

          ;; The following is generated automatically from colorwheel.py
          ;;;; THEMIAN-COLORS-START
          (base-5   (if dark "#111213" "#fdf5dd"))
          (base-4   (if dark "#18191b" "#f7eed4"))
          (base-3   (if dark "#242628" "#e5d9b2"))
          (base-2   (if dark "#3d3f43" "#c9b882"))
          (base-1   (if dark "#61656b" "#baa35e"))
          (base+1   (if dark "#94989e" "#857547"))
          (base+2   (if dark "#bcbfc2" "#53492d"))
          (base+3   (if dark "#d7d8db" "#2e2a1f"))
          (base+4   (if dark "#e4e5e7" "#1c1b17"))
          (blue     (if dark "#7596c8" "#37588a"))
          (green    (if dark "#7ac875" "#3c8a37"))
          (yellow   (if dark "#c8c375" "#8a8537"))
          (violet   (if dark "#8b75c8" "#4d378a"))
          (orange   (if dark "#c8b575" "#8a7737"))
          (cyan     (if dark "#75c8c3" "#378a85"))
          (magenta  (if dark "#c875b2" "#8a3774"))
          (red      (if dark "#c87588" "#8a374a"))
          (blue-bg  (if dark "#37588a" "#7596c8"))
          (green-bg (if dark "#3c8a37" "#7ac875"))
          (yellow-bg (if dark "#8a8537" "#c8c375"))
          (violet-bg (if dark "#4d378a" "#8b75c8"))
          (orange-bg (if dark "#8a7737" "#c8b575"))
          (cyan-bg  (if dark "#378a85" "#75c8c3"))
          (magenta-bg (if dark "#8a3774" "#c875b2"))
          (red-bg   (if dark "#8a374a" "#c87588"))
          (diff-1   (if dark "#1d4946" "#92d3cf"))
          (diff-2   (if dark "#33807a" "#5bbdb7"))
          (diff-3   (if dark "#071211" "#c8e9e7"))
          (diff-4   (if dark "#1d4946" "#92d3cf"))
          ;;;; THEMIAN-COLORS-END
          )
     (mapcar
      (lambda (config)
          `(,(nth 0 config) ((t ,(nth 1 config)))))
      ,@body)))


(defun themian-create-color-theme (theme-name variant)
  (apply
   #'custom-theme-set-faces
   theme-name
   (themian-with-color-variables
     variant
     `(
       (default (:foreground ,base+3 :background ,base-4 :weight normal :box nil
                             :underline nil :slant normal :overline nil
                             :strike-through nil))
       (button (:foreground ,base+4 :background ,base-2
                            :box (:line-width 1 :style released-button)))
       (cursor (:background ,cyan))
       (escape-glyph (:foreground ,base+3 :background ,base-2 :weight bold))
       (font-lock-builtin-face (:foreground ,blue))
       (font-lock-comment-delimiter-face (:inherit font-lock-comment-face))
       (font-lock-comment-face (:foreground ,cyan :slant italic))
       (font-lock-constant-face (:foreground ,green))
       (font-lock-doc-face (:foreground ,green))
       (font-lock-function-name-face (:foreground ,blue))
       (font-lock-keyword-face (:foreground ,orange))
       (font-lock-negation-char-face (:inherit default))
       (font-lock-preprocessor-face (:foreground ,orange))
       (font-lock-regexp-grouping-backslash (:foreground ,base+1))
       (font-lock-regexp-grouping-construct (:foreground ,base+2))
       (font-lock-string-face (:foreground ,base+2))
       (font-lock-type-face (:foreground ,green))
       (font-lock-variable-name-face (:foreground ,base+4))
       (font-lock-warning-face (:foreground ,base+4 :background ,red))
       (fringe (:foreground ,base+1 :background ,base-5 :box (:line-width 1 :color ,blue)))
       (header-line (:background ,base-3))
       (highlight (:background ,base-3))
       (lazy-highlight (:inherit default))
       (link (:foreground ,orange :underline t))
       (link-visited (:background ,unknown))
       (line-number (:inherit linum))
       (line-number-current-line (:inherit linum-relative-current-face))
       (linum
        (:foreground ,base-1 :background ,base-5 :weight normal :slant italic
                     :underline nil :box nil))
       (linum-relative-current-face (:foreground ,base+1 :weight bold :inherit linum))
       (minibuffer-prompt (:weight bold))
       (region (:background ,base-3 :box (:line-width -1 :color ,blue)))
       (secondary-selection (:background ,base-2))
       (shadow (:foreground ,blue))
       (tooltip (:background ,unknown))
       (trailing-whitespace (:background ,yellow))
       (warning (:foreground ,orange :weight bold))

       (avy-background-face (:foreground ,base+2))
       (avy-goto-char-timer-face (:background ,unknown))
       (avy-lead-face (:foreground ,base-3 :background ,green))
       (avy-lead-face-0 (:foreground ,base+4 :background ,blue))
       (avy-lead-face-1 (:background ,unknown))
       (avy-lead-face-2 (:foreground ,base+4 :background ,red))
       (company-echo (:foreground ,unknown))
       (company-echo-common (:foreground ,unknown))
       (company-preview (:foreground ,unknown))
       (company-preview-common (:foreground ,blue :slant italic))
       (company-preview-search (:foreground ,unknown))
       (company-scrollbar-bg (:background ,base-3))
       (company-scrollbar-fg (:background ,cyan-bg))
       (company-template-field (:foreground ,unknown))
       (company-tooltip (:background ,base-5))
       (company-tooltip-annotation (:foreground ,base+1))
       (company-tooltip-annotation-selection (:inherit company-tooltip-annotation))
       (company-tooltip-common (:weight bold))
       (company-tooltip-common-selection (:inherit company-tooltip-common))
       (company-tooltip-mouse (:foreground ,unknown))
       (company-tooltip-search (:foreground ,unknown))
       (company-tooltip-search-selection (:foreground ,unknown))
       (company-tooltip-selection (:foreground ,blue :background ,base-3))
       (csv-separator-face (:foreground ,base+1))
       (diff-header (:background ,unknown))
       (diff-refine-added (:box (:line-width -1 :color ,blue)))
       (diff-refine-changed (:background ,unknown))
       (diff-refine-removed (:inherit diff-refine-added))
       (ediff-current-diff-A (:background ,diff-3))
       (ediff-current-diff-Ancestor (:inherit ediff-current-diff-A))
       (ediff-current-diff-B (:inherit ediff-current-diff-A))
       (ediff-current-diff-C (:inherit ediff-current-diff-A))
       (ediff-even-diff-A (:background ,base-2))
       (ediff-even-diff-Ancestor (:inherit ediff-even-diff-A))
       (ediff-even-diff-B (:inherit ediff-even-diff-A))
       (ediff-even-diff-C (:inherit ediff-even-diff-A))
       (ediff-fine-diff-A (:foreground ,base+4 :background ,diff-1))
       (ediff-fine-diff-Ancestor (:inherit ediff-fine-diff-A))
       (ediff-fine-diff-B (:inherit ediff-fine-diff-A))
       (ediff-fine-diff-C (:inherit ediff-fine-diff-A))
       (ediff-odd-diff-A (:background ,base-2))
       (ediff-odd-diff-Ancestor (:inherit ediff-odd-diff-A))
       (ediff-odd-diff-B (:inherit ediff-odd-diff-A))
       (ediff-odd-diff-C (:inherit ediff-odd-diff-A))
       (evil-ex-commands (:background ,unknown))
       (evil-ex-info (:foreground ,base+1))
       (evil-ex-lazy-highlight (:inherit evil-ex-search))
       (evil-ex-search (:foreground ,green :background ,base-2 :weight bold))
       (evil-ex-substitute-matches (:inherit evil-ex-search))
       (evil-ex-substitute-replacement (:foreground ,blue :background ,base-3))
       (evil-search-highlight-persist-highlight-face (:inherit evil-ex-search))
       (flycheck-error (:underline ,red))
       (flycheck-error-list-checker-name (:foreground ,base+1))
       (flycheck-error-list-column-number (:foreground ,base+1))
       (flycheck-error-list-error (:inherit flycheck-error))
       (flycheck-error-list-highlight (:inherit highlight))
       (flycheck-error-list-id (:foreground ,blue))
       (flycheck-error-list-id-with-explainer
        (:box (:line-width 1 :style released-button)))
       (flycheck-error-list-info (:inherit flycheck-info))
       (flycheck-error-list-line-number (:foreground ,base+3))
       (flycheck-error-list-warning (:inherit flycheck-warning))
       (flycheck-fringe-error (:foreground ,red))
       (flycheck-fringe-info (:foreground ,green))
       (flycheck-fringe-warning (:foreground ,yellow))
       (flycheck-info (:underline ,green))
       (flycheck-warning (:underline ,yellow))
       (flyspell-duplicate (:underline (:color ,yellow :style wave)))
       (flyspell-incorrect (:underline (:color ,red :style wave)))
       (helm-M-x-key
        (:foreground ,base+4 :background ,base-2
                     :box (:line-width 1 :style released-button)))
       (helm-action (:foreground ,blue))
       ;; helm-bookmark-addressbook is used in helm-swoop edit buffers
       (helm-bookmark-addressbook (:inherit font-lock-comment-face))
       (helm-bookmark-directory (:inherit helm-ff-directory))
       (helm-bookmark-file (:inherit helm-ff-file))
       (helm-bookmark-gnus (:background ,unknown))
       (helm-bookmark-info (:background ,unknown))
       (helm-bookmark-man (:background ,unknown))
       (helm-bookmark-w3m (:background ,unknown))
       (helm-buffer-directory (:inherit helm-ff-directory))
       (helm-buffer-file (:inherit helm-ff-file))
       (helm-buffer-not-saved (:weight bold))
       (helm-buffer-process (:foreground ,blue))
       (helm-buffer-saved-out (:foreground ,yellow :weight bold))
       (helm-buffer-size (:foreground ,base+2))
       (helm-candidate-number (:background ,unknown))
       (helm-etags-file (:background ,unknown))
       (helm-ff-directory (:foreground ,blue :weight bold))
       (helm-ff-dirs (:background ,unknown))
       (helm-ff-dotted-directory (:inherit helm-ff-directory))
       (helm-ff-dotted-symlink-directory (:inherit helm-ff-directory))
       (helm-ff-executable (:foreground ,green :weight bold))
       (helm-ff-file (:foreground ,base+4))
       (helm-ff-invalid-symlink (:inherit font-lock-warning-face))
       (helm-ff-prefix (:foreground ,base+4 :weight bold))
       (helm-ff-symlink (:foreground ,green :weight bold))
       (helm-grep-cmd-line (:foreground ,orange))
       (helm-grep-file (:foreground ,blue))
       (helm-grep-finish (:slant italic))
       (helm-grep-lineno (:foreground ,green))
       (helm-grep-match (:inherit helm-match))
       (helm-header (:background ,unknown))
       (helm-header-line-left-margin (:foreground ,yellow :weight bold))
       (helm-helper (:background ,unknown))
       (helm-history-deleted (:background ,unknown))
       (helm-history-remote (:background ,unknown))
       (helm-lisp-completion-info (:background ,unknown))
       (helm-lisp-show-completion (:background ,unknown))
       (helm-locate-finish (:background ,unknown))
       (helm-match (:foreground ,green :weight bold))
       (helm-match-item (:inherit helm-match))
       (helm-moccur-buffer (:inherit helm-grep-file))
       (helm-prefarg (:background ,unknown))
       (helm-resume-need-update (:background ,unknown))
       (helm-selection (:inherit highlight))
       (helm-selection-line (:inherit highlight))
       (helm-separator (:foreground ,blue))
       (helm-source-header (:foreground ,blue :weight bold :height 1.15))
       (helm-swoop-line-number-face (:background ,unknown))
       (helm-swoop-target-line-block-face (:background ,unknown))
       (helm-swoop-target-line-face (:inherit highlight))
       (helm-swoop-target-word-face (:inherit isearch))
       (helm-visible-mark (:foreground ,orange :weight bold))
       (hydra-face-amaranth (:foreground ,violet))
       (hydra-face-blue (:foreground ,cyan))
       (hydra-face-pink (:foreground ,magenta))
       (hydra-face-red (:foreground ,red))
       (hydra-face-teal (:foreground ,blue))
       (isearch (:foreground ,green :background ,base-2 :weight bold))
       (isearch-fail (:foreground ,orange :background ,base-3))
       (magit-bisect-bad (:foreground ,red :weight bold))
       (magit-bisect-good (:foreground ,green :weight bold))
       (magit-bisect-skip (:foreground ,base+4 :weight bold))
       (magit-blame-date (:foreground ,blue :background ,base-2))
       (magit-blame-hash (:background ,unknown))
       (magit-blame-heading (:background ,base-2))
       (magit-blame-name
        (:foreground ,base+3 :background ,base-2 :weight normal))
       (magit-blame-summary
        (:foreground ,base+4 :background ,base-2 :slant italic))
       (magit-branch-current
        (:foreground ,blue :weight bold :box (:line-width 1)))
       (magit-branch-local (:foreground ,green :weight bold))
       (magit-branch-remote (:foreground ,cyan :weight bold))
       (magit-cherry-equivalent (:background ,unknown))
       (magit-cherry-unmatched (:background ,unknown))
       (magit-diff-added (:background ,diff-1))
       (magit-diff-added-highlight (:foreground ,base+4 :background ,diff-2))
       (magit-diff-base (:background ,unknown))
       (magit-diff-base-highlight (:background ,unknown))
       (magit-diff-conflict-heading (:inherit magit-diff-hunk-heading))
       (magit-diff-context (:background ,base-4))
       (magit-diff-context-highlight (:background ,base-4))
       (magit-diff-file-heading (:foreground ,blue))
       (magit-diff-file-heading-highlight (:inherit highlight))
       (magit-diff-file-heading-selection (:background ,unknown))
       (magit-diff-hunk-heading
        (:foreground ,base+2 :background ,base-2 :slant italic))
       (magit-diff-hunk-heading-highlight
        (:foreground ,base+4 :background ,base-2 :slant normal))
       (magit-diff-hunk-heading-selection (:background ,unknown))
       (magit-diff-lines-boundary (:background ,base+1))
       (magit-diff-lines-heading (:foreground ,base-4 :background ,blue))
       (magit-diff-our (:background ,diff-3))
       (magit-diff-our-highlight (:background ,diff-4))
       (magit-diff-removed (:background ,diff-3))
       (magit-diff-removed-highlight (:foreground ,base+4 :background ,diff-4))
       (magit-diff-their (:background ,diff-1))
       (magit-diff-their-highlight (:background ,diff-2))
       (magit-diff-whitespace-warning (:background ,unknown))
       (magit-diffstat-added (:foreground ,green))
       (magit-diffstat-removed (:foreground ,red))
       (magit-dimmed (:foreground ,base+4))
       (magit-filename (:inherit default))
       (magit-hash (:foreground ,base+1))
       (magit-head (:foreground ,blue :weight bold))
       (magit-header-line (:foreground ,orange :weight bold))
       (magit-log-author (:foreground ,blue))
       (magit-log-date (:foreground ,green))
       (magit-log-graph (:inherit default))
       (magit-popup-argument (:foreground ,base+4 :weight bold))
       (magit-popup-disabled-argument (:foreground ,base+4))
       (magit-popup-heading (:foreground ,orange :weight bold))
       (magit-popup-key (:foreground ,blue))
       (magit-popup-option-value (:weight bold))
       (magit-process-ng (:foreground ,red :weight bold))
       (magit-process-ok (:inherit default))
       (magit-reflog-amend (:foreground ,orange))
       (magit-reflog-checkout (:foreground ,green))
       (magit-reflog-cherry-pick (:background ,unknown))
       (magit-reflog-commit (:foreground ,base+2))
       (magit-reflog-merge (:foreground ,green))
       (magit-reflog-other (:foreground ,red))
       (magit-reflog-rebase (:foreground ,yellow))
       (magit-reflog-remote (:foreground ,blue))
       (magit-reflog-reset (:foreground ,blue))
       (magit-refname (:background ,unknown))
       (magit-refname-stash (:background ,unknown))
       (magit-refname-wip (:background ,unknown))
       (magit-section-heading (:foreground ,orange :weight bold))
       (magit-section-heading-selection (:background ,unknown))
       (magit-section-highlight (:inherit highlight))
       (magit-section-secondary-heading (:foreground ,orange))
       (magit-sequence-done (:inherit magit-sequence-head))
       (magit-sequence-drop (:background ,unknown))
       (magit-sequence-head (:foreground ,base+4))
       (magit-sequence-onto (:foreground ,base+4 :weight bold))
       (magit-sequence-part (:foreground ,orange))
       (magit-sequence-pick (:foreground ,green))
       (magit-sequence-stop (:background ,unknown))
       (magit-signature-bad (:background ,unknown))
       (magit-signature-good (:background ,unknown))
       (magit-signature-untrusted (:background ,unknown))
       (magit-tag (:foreground ,yellow :weight bold :box t))
       (mode-line
        (:background ,base-3 :foreground ,base+2
                     :box (:line-width -1 :color ,base-3 :style released-button)))
       (mode-line-buffer-id (:foreground ,base+4 :background ,base-3
                                         :inherit mode-line))
       (mode-line-emphasis (:foreground ,yellow :background ,base-3
                                        :weight bold))
       (mode-line-highlight (:foreground ,base-4 :background ,yellow))
       (mode-line-inactive
        (:background ,base-3 :foreground ,base+1 :inherit mode-line))
       (mu4e-attach-number-face (:foreground ,blue :weight bold))
       (mu4e-cited-1-face (:foreground ,cyan))
       (mu4e-cited-2-face (:foreground ,unknown))
       (mu4e-cited-3-face (:foreground ,unknown))
       (mu4e-cited-4-face (:foreground ,unknown))
       (mu4e-cited-5-face (:foreground ,unknown))
       (mu4e-cited-6-face (:foreground ,unknown))
       (mu4e-cited-7-face (:foreground ,unknown))
       (mu4e-compose-header-face (:foreground ,unknown))
       (mu4e-compose-separator-face (:foreground ,unknown))
       (mu4e-contact-face (:foreground ,blue))
       (mu4e-context-face (:foreground ,blue :weight bold))
       (mu4e-draft-face (:foreground ,unknown))
       (mu4e-flagged-face (:foreground ,unknown))
       (mu4e-footer-face (:foreground ,unknown))
       (mu4e-forwarded-face (:foreground ,unknown))
       (mu4e-header-face (:foreground ,base+4))
       (mu4e-header-highlight-face (:inherit highlight))
       (mu4e-header-key-face (:foreground ,base+2))
       (mu4e-header-marks-face (:foreground ,unknown))
       (mu4e-header-title-face (:foreground ,unknown))
       (mu4e-header-value-face (:foreground ,base+4))
       (mu4e-highlight-face (:inherit highlight))
       (mu4e-link-face (:foreground ,base+4 :weight bold :underline t))
       (mu4e-maildirs-extension-maildir-face (:foreground ,base+1))
       (mu4e-maildirs-extension-maildir-hl-face (:foreground ,base+2 :weight bold))
       (mu4e-modeline-face (:foreground ,base+4))
       (mu4e-moved-face (:foreground ,unknown))
       (mu4e-ok-face (:foreground ,unknown))
       (mu4e-region-code (:foreground ,unknown))
       (mu4e-replied-face (:foreground ,unknown))
       (mu4e-special-header-value-face (:foreground ,cyan))
       (mu4e-system-face (:foreground ,base+1 :slant italic))
       (mu4e-title-face (:foreground ,yellow :weight bold))
       (mu4e-trashed-face (:foreground ,unknown))
       (mu4e-unread-face (:foreground ,yellow :weight bold))
       (mu4e-url-number-face (:foreground ,unknown))
       (mu4e-view-body-face (:inherit default))
       (mu4e-warning-face (:foreground ,unknown))
       (org-agenda-calendar-event (:inherit default))
       (org-agenda-calendar-sexp (:foreground ,green :weight bold))
       (org-agenda-clocking (:background ,base-3 :box (:line-width -1 :color ,blue)))
       (org-agenda-column-dateline (:background ,unknown))
       (org-agenda-current-time (:foreground ,base+4 :weight bold))
       (org-agenda-date (:foreground ,blue))
       (org-agenda-date-today (:foreground ,base+4 :weight bold))
       (org-agenda-date-weekend (:foreground ,base+2 :slant italic))
       (org-agenda-diary (:background ,unknown))
       (org-agenda-dimmed-todo-face (:foreground ,base+2 :slant italic))
       (org-agenda-done (:foreground ,base+2 :slant italic))
       (org-agenda-filter-category (:background ,unknown))
       (org-agenda-filter-regexp (:background ,unknown))
       (org-agenda-filter-tags (:foreground ,base+3 :background ,base-4 :weight bold))
       (org-agenda-restriction-lock (:background ,unknown))
       (org-agenda-structure (:foreground ,orange :weight bold))
       (org-archived (:background ,unknown))
       (org-block (:background ,base-5))
       (org-block-begin-line (:foreground ,base+1 :background ,base-4 :slant italic))
       (org-block-end-line (:inherit org-block-begin-line))
       (org-checkbox (:foreground ,base+4 :weight bold))
       (org-checkbox-statistics-done (:foreground ,base+4 :slant italic))
       (org-checkbox-statistics-todo (:foreground ,yellow :weight bold))
       (org-clock-overlay (:background ,unknown))
       (org-code (:foreground ,base+3 :background ,base-5))
       (org-column (:foreground ,base+4 :weight bold :background ,base-3))
       (org-column-title (:foreground ,yellow :background ,base-2 :weight bold))
       (org-date (:foreground ,cyan))
       (org-date-selected (:foreground ,base-4 :background ,cyan))
       (org-default (:background ,unknown))
       (org-document-info (:inherit default))
       (org-document-info-keyword (:foreground ,blue :slant italic))
       (org-document-title (:foreground ,base+4 :weight bold :height 1.2))
       (org-done (:foreground ,green :weight bold :strike-through t))
       (org-drawer (:background ,unknown))
       (org-ellipsis (:foreground ,base-1))
       (org-footnote (:background ,unknown))
       (org-formula (:background ,unknown))
       (org-habit-alert-face (:foreground ,base+4 :background ,yellow-bg))
       (org-habit-alert-future-face (:inherit org-habit-alert-face))
       (org-habit-clear-face (:foreground ,base+3 :background ,blue-bg))
       (org-habit-clear-future-face (:inherit org-habit-clear-face))
       (org-habit-overdue-face (:foreground ,base-3 :background ,red-bg))
       (org-habit-overdue-future-face (:inherit org-habit-overdue-face))
       (org-habit-ready-face (:foreground ,base+3 :background ,green-bg))
       (org-habit-ready-future-face (:inherit org-habit-ready-face))
       (org-headline-done (:background ,base-4 :inherit org-done))
       (org-hide (:foreground ,base-4 :background ,base-4))
       (org-indent (:foreground ,base-4 :background ,base-4))
       (org-kbd (:foreground ,orange))
       (org-latex-and-related (:background ,unknown))
       (org-level-1 (:foreground ,orange :height 1.1))
       (org-level-2 (:foreground ,blue   :height 1.1))
       (org-level-3 (:foreground ,green  :height 1.1))
       (org-level-4 (:foreground ,cyan   :height 1.1))
       (org-level-5 (:inherit org-level-1))
       (org-level-6 (:inherit org-level-2))
       (org-level-7 (:inherit org-level-3))
       (org-level-8 (:inherit org-level-4))
       (org-link (:weight bold :underline t))
       (org-list-dt (:foreground ,blue))
       (org-macro (:background ,unknown))
       (org-meta-line (:foreground ,blue))
       (org-mode-line-clock (:background ,unknown))
       (org-mode-line-clock-overrun (:background ,unknown))
       (org-priority (:foreground ,cyan :weight bold))
       (org-property-value (:weight bold))
       (org-quote (:background ,base-5 :slant italic))
       (org-scheduled (:foreground ,base+4))
       (org-scheduled-previously (:foreground ,orange))
       (org-scheduled-today (:foreground ,yellow))
       (org-sexp-date (:foreground ,green :background ,base-4 :slant italic))
       (org-special-keyword (:foreground ,base+1))
       (org-table (:foreground ,cyan))
       (org-tag (:foreground ,base+3 :weight bold))
       (org-tag-group (:background ,unknown))
       (org-target (:background ,unknown))
       (org-time-grid (:foreground ,base+3 :slant italic))
       (org-todo (:foreground ,yellow :weight bold))
       (org-upcoming-deadline (:foreground ,base+4))
       (org-verbatim (:inherit org-code))
       (org-verse (:background ,unknown))
       (org-warning (:foreground ,red))
       (paren-face-match (:background ,red))
       (paren-face-mismatch (:background ,yellow))
       (paren-face-no-match (:background ,blue))
       (powerline-active1
        (:background ,base-4 :box (:line-width -1 :color ,base-3 :style released-button)))
       (powerline-active2
        (:background ,base-2 :box (:line-width -1 :color ,base-3 :style released-button)))
       (powerline-inactive1 (:inherit powerline-active1))
       (powerline-inactive2 (:background ,base-4 :inherit powerline-active2))
       (spaceline-modified (:background ,unknown))
       (rainbow-delimiters-depth-1-face (:foreground ,base+4))
       (rainbow-delimiters-depth-10-face (:foreground ,base+3))
       (rainbow-delimiters-depth-11-face (:foreground ,base+4))
       (rainbow-delimiters-depth-12-face (:foreground ,base+3))
       (rainbow-delimiters-depth-2-face (:foreground ,base+3))
       (rainbow-delimiters-depth-3-face (:foreground ,base+4))
       (rainbow-delimiters-depth-4-face (:foreground ,base+3))
       (rainbow-delimiters-depth-5-face (:foreground ,base+4))
       (rainbow-delimiters-depth-6-face (:foreground ,base+3))
       (rainbow-delimiters-depth-7-face (:foreground ,base+4))
       (rainbow-delimiters-depth-8-face (:foreground ,base+3))
       (rainbow-delimiters-depth-9-face (:foreground ,base+4))
       (show-paren-match (:inverse-video t :weight normal))
       (show-paren-mismatch (:foreground ,red :background ,base-2))
       (spacemacs-emacs-face (:background ,blue :inherit powerline-active1))
       (spacemacs-evilified-face (:background ,blue :inherit powerline-active1))
       (spacemacs-helm-navigation-ms-face (:background ,unknown))
       (spacemacs-hybrid-face (:background ,unknown))
       (spacemacs-ido-navigation-ms-face (:background ,unknown))
       (spacemacs-iedit-face (:background ,unknown))
       (spacemacs-iedit-insert-face (:background ,unknown))
       (spacemacs-insert-face
        (:foreground ,base-4 :background ,green :weight bold
                     :inherit powerline-active1))
       (spacemacs-lisp-face (:background ,unknown))
       ;; spacemacs-micro-state-binding-face is being overwritten somewhere
       (spacemacs-micro-state-binding-face (:foreground ,cyan
                                                        :inherit powerline-active1))
       (spacemacs-micro-state-header-face (:background ,unknown))
       (spacemacs-mode-line-new-version-lighter-error-face (:background ,red))
       (spacemacs-mode-line-new-version-lighter-success-face
        (:background ,yellow
                     :box (:line-width -1 :color "#0000ff" :style released-button)))
       (spacemacs-mode-line-new-version-lighter-warning-face
        (:background ,orange))
       (spacemacs-motion-face
        (:foreground ,base-4 :background ,blue :weight bold
                     :inherit powerline-active1))
       (spacemacs-normal-face
        (:foreground ,base-4 :background ,base+3 :weight bold
                     :inherit powerline-active1))
       (spacemacs-replace-face
        (:foreground ,base+3 :background ,red :weight bold
                     :inherit powerline-active1))
       (spacemacs-visual-face
        (:foreground ,base-4 :background ,blue :weight bold
                     :inherit powerline-active1))
       (undo-tree-visualizer-active-branch-face
        (:foreground ,base+4 :weight bold))
       (undo-tree-visualizer-current-face (:foreground ,cyan :weight bold))
       (undo-tree-visualizer-default-face (:foreground ,base+2 :slant italic))
       (undo-tree-visualizer-register-face (:foreground ,unknown))
       (undo-tree-visualizer-unmodified-face (:foreground ,green))
       (whitespace-empty (:foreground ,unknown))
       (whitespace-hspace (:foreground ,unknown))
       (whitespace-indentation (:inherit whitespace-space :weight bold))
       (whitespace-line (:foreground ,base+4 :background ,base-2))
       (whitespace-newline (:inherit whitespace-space))
       (whitespace-space (:foreground ,base+1))
       (whitespace-space-after-tab (:inherit whitespace-trailing))
       (whitespace-space-before-tab (:inherit whitespace-trailing))
       (whitespace-tab (:inherit whitespace-space))
       (whitespace-trailing (:foreground ,base-3 :background ,yellow))
       ))))


;;;###autoload
(when load-file-name
	(add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide 'themian-theme)

;;; themian-theme.el ends here
