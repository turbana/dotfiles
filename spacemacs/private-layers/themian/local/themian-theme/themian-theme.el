;;; themian-theme.el --- My color theme

;;; Commentary:

;;; Code:

(deftheme themian
  "My color theme")


(defmacro themian-with-color-variables (&rest body)
  "Bind all variables in `themian-colors-alist'.

  Taken from zenburn-theme.el"
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (color)
                     (list (nth 0 color) (nth 1 color)))
                   themian-colors-list))
     ,@body))


(setq base0   "#181c22"
      base1   "#22262d"
      base2   "#5a5f66"
      base3   "#666b73"
      base4   "#7f848c"
      base5   "#8c9199"
      base6   "#d0d5de"
      base7   "#dee3ec"
      base "#008dda"
      green "#20da00"
      yellow "#daba00"
      violet "#0020da"
      orange "#da4d00"
      cyan "#00daba"
      magenta "#ba00da"
      red "#da008d"
      )


(setq themian-colors-list
      `((unknown  "#9933ff" "#9933ff")
        (diff-1   "#204040" "#66cccc")
        (diff-2   "#306060" "#6cd9d9")
        (diff-3   "#102020" "#59b2b2")
        (diff-4   "#183030" "#60bfbf")
        (base-4   ,base0    ,base7)
        (base-3   ,base1    ,base6)
        (base-2   ,base2    ,base5)
        (base-1   ,base3    ,base4)
        (base+1   ,base4    ,base3)
        (base+2   ,base5    ,base2)
        (base+3   ,base6    ,base1)
        (base+4   ,base7    ,base0)
        (blue     ,base     ,base)
        (orange   ,orange   ,orange)
        (red      ,red      ,red)
        (magenta  ,magenta  ,magenta)
        (green    ,green    ,green)
        (violet   ,violet   ,violet)
        (cyan     ,cyan     ,cyan)
        (yellow   ,yellow   ,yellow)))


(themian-with-color-variables
 (setq
  themian-faces-list
  `(
    (button (:foreground ,base+4 :background ,base-2
                         :box (:line-width 1 :style released-button)))
    (cursor (:background ,cyan))
    (default (:foreground ,base+3 :background ,base-4 :weight normal))
    (escape-glyph (:foreground ,base+3 :background ,base-2 :weight bold))
    (font-lock-builtin-face (:foreground ,blue))
    (font-lock-comment-delimiter-face (:inherit font-lock-comment-face))
    (font-lock-comment-face (:foreground ,cyan :slant italic))
    (font-lock-constant-face (:foreground ,green))
    (font-lock-doc-face (:foreground ,green))
    (font-lock-function-name-face (:foreground ,blue))
    (font-lock-keyword-face (:foreground ,yellow))
    (font-lock-negation-char-face (:inherit default))
    (font-lock-preprocessor-face (:foreground ,yellow))
    (font-lock-regexp-grouping-backslash (:background ,unknown))
    (font-lock-regexp-grouping-construct (:background ,unknown))
    (font-lock-string-face (:foreground ,base+2))
    (font-lock-type-face (:foreground ,green))
    (font-lock-variable-name-face (:foreground ,base+4))
    (font-lock-warning-face (:foreground ,base+4 :background ,red))
    (fringe (:foreground ,base+1 :background ,base-3))
    (header-line (:background ,base-3))
    (highlight (:background ,base-3))
    (lazy-highlight (:inherit normal))
    (link (:foreground ,yellow :underline t))
    (link-visited (:background ,unknown))
    (linum
     (:foreground ,base+1 :background ,base-3 :weight normal :slant normal
                  :underline nil))
    (minibuffer-prompt (:weight bold))
    (region (:background ,base-2))
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
    (helm-grep-cmd-line (:foreground ,yellow))
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
    (magit-header-line (:foreground ,yellow :weight bold))
    (magit-log-author (:foreground ,blue))
    (magit-log-date (:foreground ,green))
    (magit-log-graph (:inherit default))
    (magit-popup-argument (:foreground ,base+4 :weight bold))
    (magit-popup-disabled-argument (:foreground ,base+4))
    (magit-popup-heading (:foreground ,yellow :weight bold))
    (magit-popup-key (:foreground ,blue))
    (magit-popup-option-value (:weight bold))
    (magit-process-ng (:foreground ,red :weight bold))
    (magit-process-ok (:inherit default))
    (magit-reflog-amend (:foreground ,yellow))
    (magit-reflog-checkout (:foreground ,green))
    (magit-reflog-cherry-pick (:background ,unknown))
    (magit-reflog-commit (:foreground ,base+2))
    (magit-reflog-merge (:foreground ,green))
    (magit-reflog-other (:background ,unknown))
    (magit-reflog-rebase (:foreground ,yellow))
    (magit-reflog-remote (:foreground ,blue :weight bold))
    (magit-reflog-reset (:foreground ,blue))
    (magit-refname (:background ,unknown))
    (magit-refname-stash (:background ,unknown))
    (magit-refname-wip (:background ,unknown))
    (magit-section-heading (:foreground ,yellow :weight bold))
    (magit-section-heading-selection (:background ,unknown))
    (magit-section-highlight (:inherit highlight))
    (magit-section-secondary-heading (:foreground ,yellow))
    (magit-sequence-done (:inherit magit-sequence-head))
    (magit-sequence-drop (:background ,unknown))
    (magit-sequence-head (:foreground ,base+4))
    (magit-sequence-onto (:foreground ,base+4 :weight bold))
    (magit-sequence-part (:foreground ,yellow))
    (magit-sequence-pick (:foreground ,green))
    (magit-sequence-stop (:background ,unknown))
    (magit-signature-bad (:background ,unknown))
    (magit-signature-good (:background ,unknown))
    (magit-signature-untrusted (:background ,unknown))
    (magit-tag (:foreground ,yellow :weight bold :box t))
    (mode-line
     (:background ,base-2
                  :box (:line-width -1 :color nil :style released-button)))
    (mode-line-buffer-id (:foreground ,base+4 :weight bold))
    (mode-line-emphasis (:background ,unknown))
    (mode-line-highlight (:foreground ,base-4 :background ,yellow))
    (mode-line-inactive
     (:background ,base-4 :foreground ,base+2 :inherit (mode-line)))
    (org-agenda-calendar-event (:inherit normal))
    (org-agenda-calendar-sexp (:foreground ,green :weight bold))
    (org-agenda-clocking (:background ,unknown))
    (org-agenda-column-dateline (:background ,unknown))
    (org-agenda-current-time (:foreground ,base+4 :weight bold))
    (org-agenda-date (:foreground ,blue))
    (org-agenda-date-today (:foreground ,base+4 :weight bold))
    (org-agenda-date-weekend (:foreground ,base+2 :slant italic))
    (org-agenda-diary (:background ,unknown))
    (org-agenda-dimmed-todo-face (:foreground ,base+1))
    (org-agenda-done (:foreground ,base+2 :slant italic))
    (org-agenda-filter-category (:background ,unknown))
    (org-agenda-filter-regexp (:background ,unknown))
    (org-agenda-filter-tags (:background ,unknown))
    (org-agenda-restriction-lock (:background ,unknown))
    (org-agenda-structure (:foreground ,yellow :weight bold))
    (org-archived (:background ,unknown))
    (org-block (:background ,base-3))
    (org-block-begin-line (:foreground ,cyan :slant italic))
    (org-block-end-line (:inherit org-block-begin-line))
    (org-checkbox (:foreground ,base+4 :weight bold))
    (org-checkbox-statistics-done (:foreground ,base+4 :slant italic))
    (org-checkbox-statistics-todo (:foreground ,yellow :weight bold))
    (org-clock-overlay (:background ,unknown))
    (org-code (:foreground ,base+4 :background ,base-2))
    (org-column (:foreground ,base+4 :weight bold :background ,base-3))
    (org-column-title (:foreground ,yellow :background ,base-2 :weight bold))
    (org-date (:foreground ,cyan))
    (org-date-selected (:foreground ,base-4 :background ,cyan))
    (org-default (:background ,unknown))
    (org-document-info (:inherit normal))
    (org-document-info-keyword (:foreground ,blue :slant italic))
    (org-document-title (:foreground ,base+4 :weight bold :height 1.2))
    (org-done (:foreground ,green :weight bold :strike-through t))
    (org-drawer (:background ,unknown))
    (org-ellipsis (:inherit normal))
    (org-footnote (:background ,unknown))
    (org-formula (:background ,unknown))
    (org-headline-done (:background ,unknown))
    (org-hide (:foreground ,base-4 :foreground ,base-4))
    (org-indent (:inherit org-hide))
    (org-kbd (:foreground ,yellow))
    (org-latex-and-related (:background ,unknown))
    (org-level-1 (:foreground ,yellow))
    (org-level-2 (:foreground ,blue))
    (org-level-3 (:foreground ,green))
    (org-level-4 (:foreground ,cyan))
    (org-level-5 (:inherit org-level-1))
    (org-level-6 (:inherit org-level-2))
    (org-level-7 (:inherit org-level-3))
    (org-level-8 (:inherit org-level-4))
    (org-link (:weight bold :underline t))
    (org-list-dt (:background ,unknown))
    (org-macro (:background ,unknown))
    (org-meta-line (:foreground ,blue))
    (org-mode-line-clock (:background ,unknown))
    (org-mode-line-clock-overrun (:background ,unknown))
    (org-priority (:foreground ,cyan :weight bold))
    (org-property-value (:weight bold))
    (org-quote (:background ,unknown))
    (org-scheduled (:foreground ,base+4))
    (org-scheduled-previously (:foreground ,orange))
    (org-scheduled-today (:foreground ,yellow))
    (org-sexp-date (:background ,unknown))
    (org-special-keyword (:foreground ,blue :slant italic))
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
    (spacemacs-emacs-face (:background ,blue))
    (spacemacs-evilified-face (:background ,blue))
    (spacemacs-helm-navigation-ms-face (:background ,unknown))
    (spacemacs-hybrid-face (:background ,unknown))
    (spacemacs-ido-navigation-ms-face (:background ,unknown))
    (spacemacs-iedit-face (:background ,unknown))
    (spacemacs-iedit-insert-face (:background ,unknown))
    (spacemacs-insert-face
     (:foreground ,base-4 :background ,green :weight bold))
    (spacemacs-lisp-face (:background ,unknown))
    ;; spacemacs-micro-state-binding-face is being overwritten somewhere
    (spacemacs-micro-state-binding-face (:foreground ,cyan))
    (spacemacs-micro-state-header-face (:background ,unknown))
    (spacemacs-mode-line-new-version-lighter-error-face (:background ,red))
    (spacemacs-mode-line-new-version-lighter-success-face
     (:background ,yellow
                  :box (:line-width -1 :color "#0000ff" :style released-button)))
    (spacemacs-mode-line-new-version-lighter-warning-face
     (:background ,orange))
    (spacemacs-motion-face
     (:foreground ,base-4 :background ,blue :weight bold))
    (spacemacs-normal-face
     (:foreground ,base-4 :background ,base+3 :weight bold))
    (spacemacs-replace-face
     (:foreground ,base+3 :background ,red :weight bold))
    (spacemacs-visual-face
     (:foreground ,base-4 :background ,blue :weight bold))
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
    )))

(let ((faces (mapcar (lambda (config)
                       `(,(nth 0 config) ((t ,(nth 1 config)))))
                     themian-faces-list)))
	(apply #'custom-theme-set-faces
         'themian
         faces))


;;;###autoload
(when load-file-name
	(add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'themian)

;;; themian-theme.el ends here
