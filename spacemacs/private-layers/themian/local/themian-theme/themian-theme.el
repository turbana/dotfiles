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


(setq themian-colors-list
      '((unknown  "#9933ff")
        (grey-1   "#181818")
        (grey-2   "#222222")
        (grey-3   "#333333")
        (grey-4   "#444444")
        (white-1  "#ffffff")
        (white-2  "#eeeeee")
        (white-3  "#999999")
        (green-1  "#66cc33")
        (green-2  "#00cccc")
        (red-1    "#993333")
        (yellow-1 "#cccc00")
        (blue-1   "#6699ff")
        (blue-2   "#99ccff")
        ))


(themian-with-color-variables
  (setq themian-faces-list
        `(
          ;; main
          (default (:foreground ,white-2 :background ,grey-1))
          (fringe (:foreground ,white-2 :background ,grey-2))
          (cursor (:background ,yellow-1))
          (highlight (:background ,grey-2))
          (region (:background ,grey-3))
          (secondary-selection (:background ,grey-4))
          (shadow (:foreground ,blue-1))
          (escape-glyph (:background ,unknown))
          (minibuffer-prompt (:weight bold))
          (trailing-whitespace (:background ,yellow-1))
          (button (:background ,unknown))
          (link (:foreground ,yellow-1 :underline t))
          (link-visited (:background ,unknown))
          (header-line (:background ,grey-2))
          (tooltip (:background ,unknown))
          ;; basic
          (font-lock-builtin-face (:foreground ,blue-2))
          (font-lock-comment-delimiter-face (:foreground ,green-2))
          (font-lock-comment-face (:foreground ,green-2))
          (font-lock-constant-face (:foreground ,green-1))
          ;; (font-lock-doc-face (:background ,grey-2))
          (font-lock-function-name-face (:foreground ,blue-1))
          (font-lock-keyword-face (:foreground ,yellow-1))
          (font-lock-negation-char-face (:background ,unknown))
          (font-lock-preprocessor-face (:foreground ,yellow-1))
          (font-lock-regexp-grouping-backslash (:background ,unknown))
          (font-lock-regexp-grouping-construct (:background ,unknown))
          (font-lock-string-face (:foreground ,white-3))
          (font-lock-type-face (:foreground ,green-1))
          (font-lock-variable-name-face (:foreground ,white-1))
          (font-lock-warning-face (:foreground ,white-1 :background ,red-1))
          ;; mode-line
          (mode-line (:background ,grey-3 :box (:line-width -1 :color nil :style released-button)))
          (mode-line-inactive (:background ,grey-1 :foreground ,white-3 :inherit (mode-line)))
          (mode-line-buffer-id (:foreground ,white-1 :weight bold))
          (mode-line-emphasis (:background ,unknown))
          (mode-line-highlight (:background ,unknown))

          ;; helm
          ;; magit
          ;; org-mode
          ;; diff

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
