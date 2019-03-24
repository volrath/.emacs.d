(require 'all-the-icons)
(require 'flycheck)
(require 'magit)

;;; Faces
;;  --------------------------------------------------------------------

(defface vlt-ml-ro-face
  '((t :foreground "#0088CC" :weight bold))
  "Face for read-only buffer in the mode-line.")

(defface vlt-ml-modified-face
  '((t :foreground "#ff6c6b" :height 0.9))
  "Face for modified buffers in the mode-line.")

(defface vlt-ml-not-modified-face
  '((t :foreground "#98be65" :height 0.9))
  "Face for not modified buffers in the mode-line.")

(defface vlt-ml-buffer-position-face
  '((t :height 0.9))
  "Face for line/column numbers in the mode-line.")

(defface vlt-ml-vc-face
  '((t :foreground "#61afef"))
  "Face for vc status in the mode-line.")

(defface vlt-ml-ok-face
  '((t :foreground "#61afef"))
  "Face for ok status in the mode-line.")

(defface vlt-ml-warning-face
  '((t :foreground "#da8548"))
  "Face for warning status in the mode-line.")

(defface vlt-ml-error-face
  '((t :foreground "#ff6c6b"))
  "Face for error status in the mode-line.")

;;; Mode Line
;;  --------------------------------------------------------------------

(defun vlt-true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defvar vlt-ml--git-last-run nil)
(defvar vlt-ml--git-cache nil)

(defmacro vlt-ml--cached-for (secs &rest body)
  "Cache for SECS the result of the evaluation of BODY."
  (declare (debug t))
  `(lambda ()
     (when (or (null vlt-ml--git-last-run)
               (> (- (time-to-seconds (current-time)) vlt-ml--git-last-run)
                  ,secs))
       (setf vlt-ml--git-cache (progn ,@body))
       (setf vlt-ml--git-last-run (time-to-seconds (current-time))))
     vlt-ml--git-cache))

(defvar vlt-selected-window nil
  "Selected window.")

(defun vlt--set-selected-window (&rest _)
  "Set the selected window."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq vlt-selected-window window))))

(add-hook 'window-configuration-change-hook #'vlt--set-selected-window)
(add-hook 'focus-in-hook #'vlt--set-selected-window)
(advice-add 'select-window :after #'vlt--set-selected-window)
(advice-add 'select-frame  :after #'vlt--set-selected-window)

(defun vlt--active-window-p ()
  "Return non-nil if the current window is active."
  (eq (selected-window) vlt-selected-window))

(defun vlt-face-when-active (face)
  "Return FACE if the window is active."
  (when (vlt--active-window-p)
    face))

(defvar vlt-modeline-position
  '(:eval (propertize ":%l:%c %p" 'face (if (vlt--active-window-p)
                                            'vlt-ml-buffer-position-face
                                          'mode-line-inactive)))
  "Mode line construct for displaying the position in the buffer.")

(defvar vlt-modeline-buffer-identification
  '(:eval (propertize "%b" 'face 'bold))
  "Mode line construct for displaying the position in the buffer.")

(defvar vlt-modeline-modified
  '(:eval (if (buffer-modified-p (current-buffer))
              (all-the-icons-faicon "floppy-o"
                                    :height 0.9
                                    :v-adjust 0
                                    :face (if (vlt--active-window-p)
                                              'vlt-ml-modified-face
                                            'mode-line-inactive))
            (all-the-icons-faicon "check"
                                  :height 0.9
                                  :v-adjust 0
                                  :face (if (vlt--active-window-p)
                                            'vlt-ml-not-modified-face
                                          'mode-line-inactive)))))

(defvar vlt-modeline-ro
  '(:eval (if buffer-read-only
              (if (vlt--active-window-p)
                  (progn
                    (propertize "RO " 'face 'vlt-ml-ro-face))
                (propertize "RO " 'face 'bold))
            "")))

(defvar vlt-buffer-coding
  '(:eval (unless (eq buffer-file-coding-system
                      (default-value 'buffer-file-coding-system))
            mode-line-mule-info)))

(defvar vlt-modeline-vc
  '(vc-mode ("   "
             (:eval (all-the-icons-faicon "code-fork"
                                          :height 0.9
                                          :v-adjust 0
                                          :face (when (vlt--active-window-p)
                                                  (vlt-ml-git-face))))
             (:eval (propertize (truncate-string-to-width vc-mode 25 nil nil "...")
                                'face (when (vlt--active-window-p)
                                        (vlt-ml-git-face)))))))

(defun vlt-modeline-flycheck-status ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when flycheck-mode
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face (vlt-face-when-active 'vlt-ml-error-face)))
                                (propertize "✔ No Issues"
                                            'face (vlt-face-when-active 'vlt-ml-ok-face))))
                   (`running     (propertize "-- Running"
                                             'face (vlt-face-when-active 'vlt-ml-warning-face)))
                   (`no-checker  (propertize "⚠ No Checker"
                                             'face (vlt-face-when-active 'vlt-ml-warning-face)))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face (vlt-face-when-active 'vlt-ml-error-face)))
                   (`interrupted (propertize "⛔ Interrupted"
                                             'face (vlt-face-when-active 'vlt-ml-error-face)))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))

(defvar vlt-ml--git-face-cached (vlt-ml--cached-for 1 (vlt-ml--git-face-intern)))

(defun vlt-ml--git-face-intern ()
  "Return the face to use based on the current repository status."
  (if (magit-git-success "diff" "--quiet")
      ;; nothing to commit because nothing changed
      (if (zerop (length (magit-git-string
                          "rev-list" (concat "origin/"
                                             (magit-get-current-branch)
                                             ".."
                                             (magit-get-current-branch)))))
          ;; nothing to push as well
          'vlt-ml-ok-face
        ;; nothing to commit, but some commits must be pushed
        'vlt-ml-warning-face)
    'vlt-ml-error-face))

(defun vlt-ml-git-face ()
  "Return the face to use based on the current repository status.
The result is cached for one second to avoid hiccups."
  (funcall vlt-ml--git-face-cached))

(defun vlt-ml-set-palette (theme)
  (cond
   ((string-prefix-p "solarized-" (symbol-name theme))
    (let ((variant (if (eq theme 'solarized-light) 'light 'dark)))
      (solarized-with-color-variables variant
        (setq vlt-ml-active-foreground s-mode-line-inactive-fg
              vlt-ml-inactive-foreground s-mode-line-inactive-fg
              vlt-ml-active-background s-mode-line-inactive-bg
              vlt-ml-inactive-background s-mode-line-inactive-bg
              vlt-ml-anzu-color "#c678dd"))))
   (t (setq vlt-ml-active-foreground (if (vlt-true-color-p) "#ccd4e3" "#d7d7d7")
            vlt-ml-inactive-foreground (if (vlt-true-color-p) "#687080" "#707070")
            vlt-ml-active-background "#222222"
            vlt-ml-inactive-background (if (vlt-true-color-p) "#1c2129" "#222222")
            vlt-ml-anzu-color "#c678dd"))))

(defun vlt-setup-mode-line-format (theme)
  (vlt-ml-set-palette theme)
  (let ((class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     theme
     ;; Mode line faces
     `(mode-line
       ((,class
         (:background ,vlt-ml-active-background
                      :height 0.9
                      :foreground ,vlt-ml-active-foreground
                      :box (:line-width 6 :color ,vlt-ml-active-background)))))
     `(mode-line-inactive
       ((,class
         (:background ,vlt-ml-inactive-background
                      :height 0.9
                      :foreground ,vlt-ml-inactive-foreground
                      :box (:line-width 6 :color ,vlt-ml-inactive-background)))))
     `(anzu-mode-line
       ((,class :inherit mode-line :foreground ,vlt-ml-anzu-color :weight bold)))))
  (setq-default mode-line-format
                `("%e"
                  " "
                  ,vlt-modeline-ro " "
                  ,vlt-buffer-coding
                  mode-line-frame-identification " "
                  " "
                  ,vlt-modeline-modified
                  " "
                  ,vlt-modeline-buffer-identification
                  ,vlt-modeline-position
                  ,vlt-modeline-vc
                  "  "
                  (:eval (vlt-modeline-flycheck-status))
                  "  " mode-line-misc-info mode-line-modes mode-line-end-spaces
                  )))

(provide 'vlt-mode-line-helpers)
