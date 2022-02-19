;;; vlt-ivy.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-ivy.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Mon Feb 14 00:48:53 2022 (+0100)
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

(require 'vlt-defaults)


(use-package flx :ensure t)


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("RET"   . ivy-alt-done)
         ("C-RET" . ivy-immediate-done)
         ("C-j"   . ivy-immediate-done))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'fullpath)
  (ivy-format-function #'ivy-format-function-arrow-line)
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))


(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-c k" . counsel-ag))
  :config (counsel-mode))


(use-package ivy-rich
  :ensure t
  :hook (ivy-mode . ivy-rich-mode)
  :init
  ;; Sprinkle some icons
  (defun ivy-rich-switch-buffer-icon (candidate)
    "Return an icon for CANDIDATE out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-switch-buffer-icon (:width 2))
                      (ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-size (:width 7))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                      (ivy-rich-switch-buffer-project (:width 15 :face success))
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                     :predicate
                     (lambda (cand) (get-buffer cand))))))


(use-package smex
  :ensure t
  :custom (smex-save-file (expand-file-name ".smex-items" vlt-defaults/backups-dir))
  :config (smex-initialize))


(use-package swiper
  :ensure t
  :init
  (defun vlt-swiper-at-point (sym)
    "Use `swiper' to search for the SYM at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("M-s /" . vlt-swiper-at-point)))

(provide 'vlt-ivy)

;;; vlt-ivy.el ends here
