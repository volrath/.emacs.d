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


;;; Font
;;  ----------------------------------------------------------------------------

(defvar vlt/fixed-width-font "Fira Code Light")
(defvar vlt/variable-width-font "Iosevka Aile")

(set-face-attribute 'default nil :font vlt/fixed-width-font :height (if (eq system-type 'darwin) 140 110))
(set-face-attribute 'fixed-pitch nil :font vlt/fixed-width-font :height 115)
(set-face-attribute 'variable-pitch nil :font vlt/variable-width-font :weight 'light :height 1.0)

(use-package all-the-icons)

(use-package emojify
  :custom
  (emojify-emoji-styles '(unicode github))
  (emojify-emojis-dir (expand-file-name "emojis" vlt/var-dir))
  :hook (after-init . global-emojify-mode))


(defun vlt/variable-pitch-mode-hook ()
  (setq line-spacing 3))


;;; Org-Mode
;; -----------------------------------------------------------------------------
;; Most of this is credited to David Wilson from SystemCrafters
;; https://systemcrafters.net/emacs-tips/presentations-with-org-present/

(require 'org-faces)

;; Style headings
(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font vlt/variable-width-font :height (cdr face)))

;; Document title should be a bit bigger
(set-face-attribute 'org-document-title nil :font vlt/variable-width-font :weight 'bold :height 1.5)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch :weight 'normal)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :weight 'normal)
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :weight 'normal)
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch :weight 'normal)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'vlt/variable-pitch-mode-hook)
(add-hook 'org-roam-mode-hook 'variable-pitch-mode)
(add-hook 'org-roam-mode-hook 'visual-line-mode)
(add-hook 'org-roam-mode-hook 'vlt/variable-pitch-mode-hook)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-hide-emphasis-markers t
      org-startup-indented t)


(use-package visual-fill-column
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :hook (org-mode org-roam-mode dashboard-mode))


(use-package org-appear
  :hook (org-mode . org-appear-mode))


(use-package org-present
  :init
  (defvar vlt/org-present--prev-windows-config nil
    "Store the windows-configuration used before goint into
`org-present' so that we can revert back to it when quitting the
presentation.")

  (defun vlt/org-present-start ()
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq org-hide-leading-stars t
          header-line-format " ")

    ;; Visual configurations
    (org-display-inline-images)
    (hl-line-mode 1)
    (fringe-mode 0)
    (smooth-scrolling-mode 0)
    (set-frame-parameter (selected-frame) 'alpha '(97 . 100))

    ;; Delete other windows but save the windows configuration to restore it
    ;; later.
    (setq vlt/org-present--prev-windows-config (current-window-configuration))
    (delete-other-windows)

    ;; Fullscreen
    (unless (memq (frame-parameter (selected-frame) 'fullscreen) '(fullscreen fullboth))
      (toggle-frame-fullscreen)))

  (defun vlt/org-present-end ()
    (setq-local face-remapping-alist '((default variable-pitch default)))
    (setq org-hide-leading-stars nil
          header-line-format nil)

    (when (memq (frame-parameter (selected-frame) 'fullscreen) '(fullscreen fullboth))
      (toggle-frame-fullscreen))

    (set-window-configuration vlt/org-present--prev-windows-config)

    (org-remove-inline-images)
    (hl-line-mode 0)
    (fringe-mode nil)
    (smooth-scrolling-mode 1)
    (set-frame-parameter (selected-frame) 'alpha '(100 . 100)))

  (defun vlt/org-present-prepare-slide (buffer-name heading)
    ;; Collapse all subheadings
    (org-overview)
    (org-show-entry)
    (org-show-children))

  :config
  (add-hook 'org-present-mode-hook 'vlt/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'vlt/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'vlt/org-present-prepare-slide)
  :bind (:map org-present-mode-keymap
              ("C-<next>" . org-present-next)
              ("C-<prior>" . org-present-prev)
              ("<left>" . left-char)
              ("<right>" . right-char)))


;;; Show clock when on fullscreen
;; -----------------------------------------------------------------------------

(defun vlt/fullscreen-display-time (&optional frame)
  (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth))
      (display-time-mode 1)
    (display-time-mode 0)))

(advice-add 'toggle-frame-fullscreen :after #'vlt/fullscreen-display-time)


;;; Extra defaults
;;  ----------------------------------------------------------------------------

(defun vlt/enable-line-visual-helpers ()
  (interactive)
  (hl-line-mode 1)
  (display-line-numbers-mode 1))

(line-number-mode t)
(column-number-mode t)
(add-hook 'prog-mode-hook 'vlt/enable-line-visual-helpers)
(add-hook 'conf-mode-hook 'vlt/enable-line-visual-helpers)
(add-hook 'nxml-mode-hook 'vlt/enable-line-visual-helpers)
(add-hook 'yaml-mode-hook 'vlt/enable-line-visual-helpers)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'git-rebase-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook 'hl-line-mode)
(add-hook 'pass-mode-hook 'hl-line-mode)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


(provide 'vlt-ui)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; vlt-ui.el ends here
