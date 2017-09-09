;;; vlt-ui.el --- 
;; 
;; Filename: vlt-ui.el
;; Description: 
;; Author: Daniel Barreto
;; Maintainer: 
;; Created: Thu Sep  7 14:15:23 2017 (+0200)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 's)

;;; Themes
;;  --------------------------------------------------------------------
;; Set custom theme path
(setq x-underline-at-descent-line t)
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(dolist (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Better looking mode-lines for solarized themes, taken from zerodark
(require 'solarized)
(defun vlt-extend-solarized-mode-line (theme-name)
  (let* ((variant (if (eq theme-name 'solarized-light) 'light 'dark)))
    (solarized-with-color-variables variant
                                    (custom-theme-set-faces
                                     theme-name
                                     `(mode-line
                                       ((,class (:inverse-video unspecified
                                                                :overline nil
                                                                :underline nil
                                                                :foreground ,s-mode-line-fg
                                                                :background ,s-mode-line-bg
                                                                :height 0.9
                                                                :box (:line-width 6 :color ,s-mode-line-bg
                                                                                  :style unspecified)
                                                                ))))
                                     `(mode-line-inactive
                                       ((,class (:inverse-video unspecified
                                                                :overline ,s-mode-line-inactive-bc
                                                                :underline nil
                                                                :foreground ,s-mode-line-inactive-fg
                                                                :background ,s-mode-line-inactive-bg
                                                                :height 0.9
                                                                :box (:line-width 6 :color ,s-mode-line-inactive-bg
                                                                                  :style unspecified)
                                                                ))))))))

;; Fonts
(setq small-font "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(setq big-font "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(setq presentation-font "-*-Hack-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")

;; Comfy theme switchers
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
  (set-face-attribute 'default nil :font font)
  (when (vlt-solarized-theme-p theme)
    (vlt-extend-solarized-mode-line theme)))

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

;; Start with the first theme in `vlt-themes'
(zerodark-setup-modeline-format)  ;; works ok in every theme
(vlt-enable-current-theme)

;; Shortcuts to cycle through themes
(global-set-key (kbd "C-<f6>") 'vlt-cycle-theme)
(global-set-key (kbd "C-<f7>") 'vlt-toggle-presentation-mode)

;;; Some behavioral stuff
;;  --------------------------------------------------------------------

;; zoom-frm, to make zoom affect frames instead of buffers
(require 'zoom-frm)

;; diminish
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(eval-after-load "subword" '(diminish 'subword-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")

;; Don't highlight matches with jump-char - it's distracting
;; TODO: (setq jump-char-lazy-highlight-face nil)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)
(smooth-scrolling-mode t)

;; Always display line and column numbers
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq linum-format "%4d")
(global-linum-mode 1)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

(provide 'vlt-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-ui.el ends here
