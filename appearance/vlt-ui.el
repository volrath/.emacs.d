;;; vlt-ui.el ---  UI stuff  -*- lexical-binding: true -*-
;; 
;; Filename: vlt-ui.el
;; Author: Daniel Barreto
;; Created: Thu Sep  7 14:15:23 2017 (+0200)
;; Package-Requires: ()
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

(require 'vlt-theme-helpers)

;;; Themes
;;  --------------------------------------------------------------------
;; Set custom theme path
(setq custom-theme-directory
      (expand-file-name "appearance/themes" user-emacs-directory))
(dolist (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Solarize customizations
(setq solarized-distinct-fringe-background t
      solarized-use-more-italic t
      solarized-scale-org-headlines nil)

;; Start with the first theme in `vlt-themes'
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

(setq x-underline-at-descent-line t)

(provide 'vlt-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-ui.el ends here
