;;; vlt-ui.el ---
;;
;; Filename: vlt-ui.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Mon Feb 14 01:47:05 2022 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;; Theme and Modeline
;;  ----------------------------------------------------------------------------

(use-package monokai-theme
  :init (load-theme 'monokai t))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  :init
  (doom-modeline-mode 1))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "clojurescript-mode" clojure-mode "Cljs")

(global-display-line-numbers-mode t)


;;; Font
;;  ----------------------------------------------------------------------------

(set-frame-font "Fira Code Light-11")
;; (set-frame-font "Hack-11")

(use-package all-the-icons)

(use-package emojify
  :custom
  (emojify-emoji-styles '(unicode github))
  (emojify-emojis-dir (expand-file-name "emojis" vlt/var-dir))
  :hook (after-init . global-emojify-mode))


;;; Show clock when on fullscreen
;; -----------------------------------------------------------------------------

(defun vlt/fullscreen-display-time (&optional frame)
  (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth))
      (display-time-mode 1)
    (display-time-mode 0)))

(advice-add 'toggle-frame-fullscreen :after #'vlt/fullscreen-display-time)


;;; Extra defaults
;;  ----------------------------------------------------------------------------

(line-number-mode t)
(column-number-mode t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


(provide 'vlt-ui)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; vlt-ui.el ends here
