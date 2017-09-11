(require 'solarized)
(require 'vlt-mode-line-helpers)

;;; Fonts
;;  --------------------------------------------------------------------
(setq small-font "-Regular-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(setq big-font "-Regular-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(setq presentation-font "-*-Hack-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")

;;; Comfy theme switchers
;;  --------------------------------------------------------------------
(setq vlt-themes `((solarized-light . ,big-font)
                   (solarized-dark . ,big-font)
                   (zerodark . ,big-font)
                   (material . ,small-font)))
(setq vlt-current-theme-index 0)

(defun vlt-solarized-theme-p (theme-name)
  (string-prefix-p "solarized" (symbol-name theme-name)))

(defun vlt-enable-theme-with-font (theme font)
  (dolist (theme-pair vlt-themes)
    (disable-theme (car theme-pair)))
  (load-theme theme)
  (vlt-setup-mode-line-format theme)
  )

(defun vlt-enable-current-theme ()
  (let* ((theme-pair (nth vlt-current-theme-index vlt-themes))
         (theme (car theme-pair))
         (font  (cdr theme-pair)))
    (vlt-enable-theme-with-font theme font)))

(defun vlt-cycle-theme ()
  (interactive)
  (let* ((themes-count (length vlt-themes))
         (next-index (mod (1+ vlt-current-theme-index) themes-count)))
    (setq vlt-current-theme-index next-index)
    (disable-theme 'prez)
    (vlt-enable-current-theme)))

(defun vlt-toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) presentation-font)
      (vlt-enable-current-theme)
    (progn
      (dolist (theme-pair vlt-themes)
        (disable-theme (car theme-pair)))
      (vlt-enable-theme-with-font 'prez presentation-font))))

(provide 'vlt-theme-helpers)
