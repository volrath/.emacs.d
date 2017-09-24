;;; vlt-general-config.el --- General Config
;;
;; Filename: vlt-general-config.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Created: Fri Sep  8 13:02:27 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 47
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

(require 'anzu)
(require 'avy)
(require 'beginend)
(require 'browse-kill-ring)
(require 'change-inner)
(require 'company)
(require 'crux)
(require 'dash)
(require 'delsel)
(require 'diminish)
(require 'editorconfig)
(require 'expand-region)
(require 'fill-column-indicator)
(require 'highlight-escape-sequences)
(require 'multiple-cursors)
(require 'operate-on-number)
(require 'org)
(require 'perspective)
(require 'prodigy)
(require 'projectile)
(require 're-builder)
(require 'rect)
(require 'smart-forward)
(require 'smartparens-config)
(require 'smartrep)
(require 'smooth-scrolling)
(require 'undo-tree)
(require 'visual-regexp)
(require 'volatile-highlights)
(require 'which-key)
(require 'whitespace)
(require 'whitespace-cleanup-mode)

;; anzu

(diminish 'anzu-mode)
(global-anzu-mode)

;; avy

;; (setq avy-background nil)
(setq avy-highlight-first t)
(setq avy-style 'at-full)

;; beginend

(beginend-global-mode)

;; browse-kill-ring

(setq browse-kill-ring-quit-action 'save-and-restore)

;; company
(setq company-idle-delay 0.5
      company-tooltip-limit 10
      company-minimum-prefix-length 2
      company-tooltip-flip-when-above t)

(defvar vlt-local-fci-mode nil
  "Local way to know if fci was activated for current buffer.")
(make-local-variable 'vlt-local-fci-mode)

(defun vlt--toggle-fci-before-company (command)
  "Turn on/off FCI when there is a company popup, given by COMMAND.
For more info see:
https://github.com/alpaker/Fill-Column-Indicator/issues/54"
  (when vlt-local-fci-mode
    (when (string= command "show")
      (turn-off-fci-mode))
    (when (string= command "hide")
      (turn-on-fci-mode))))
(advice-add 'company-call-frontends :before #'vlt--toggle-fci-before-company)

(global-company-mode 1)

;; editorconfig

(editorconfig-mode t)

;; expand-region

(setq expand-region-fast-keys-enabled nil)  ; Don't use fast keys
(setq er--show-expansion-message t)  ; Show expand-region command used

;; fill-column-indicator

(setq fci-rule-color "#111122")
(add-hook 'fci-mode-hook #'(lambda () (setq-local vlt-local-fci-mode t)))

;; highlight-escape-sequences

(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; makefile-mode: Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; operate-on-number

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

;; org mode
(setq org-directory "~/projects/life"
      org-default-notes-file (expand-file-name "agenda.org" org-directory)
      org-hide-leading-stars t
      org-return-follows-link t
      org-log-into-drawer t
      org-startup-folded nil)
(add-to-list 'org-agenda-files org-default-notes-file)
(define-obsolete-function-alias 'org-define-error 'define-error)

;; perspective
(persp-mode t)

;; projectile

(setq projectile-cache-file (expand-file-name  "projectile.cache" vlt-backups-dir))
(projectile-mode t)

;; re-builder

(setq reb-re-syntax 'string)

;; undo-tree

(global-undo-tree-mode)
(setq undo-tree-mode-lighter ""
      undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
      undo-tree-auto-save-history t)
(diminish 'undo-tree-mode)

;; volatile-highlights

(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)
;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(crux-with-region-or-line kill-region)

;; which-key

(which-key-mode)
(setq which-key-sort-order 'which-key-prefix-then-key-order)
(setq which-key-delay 0.5)
(define-key which-key-mode-map (kbd "C-S-h") 'which-key-C-h-dispatch)

;; whitespace-mode config

(setq whitespace-line-column 80) ;; general limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; whitespace-cleanup-mode

(global-whitespace-cleanup-mode)

;; yasnippets: Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)

;;; Some other big configurations that needed their own files
(require 'vlt-config-dired)
(require 'vlt-config-flycheck)
(require 'vlt-config-grep)
(require 'vlt-config-header2)
(require 'vlt-config-hippie)
(require 'vlt-config-ivy)
(require 'vlt-config-magit)
(require 'vlt-config-smartparens)
(require 'vlt-config-yasnippet)

(provide 'vlt-general-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-general-config.el ends here
