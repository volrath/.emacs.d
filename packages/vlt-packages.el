;;; vlt-packages.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-packages.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sat Feb 12 11:14:51 2022 (+0100)
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

(use-package bind-key :ensure t)
(use-package diminish :ensure t)
(require 'bind-key)
(require 'diminish)


(use-package ace-window
  :ensure t
  :bind (("M-z" . ace-window)))


(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode))


(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (("M-%" . 'anzu-query-replace)
         ("C-M-%" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-line)))


(use-package browse-kill-ring
  :ensure t
  :bind (("C-x C-y" . browse-kill-ring)))


(use-package change-inner
  :ensure t
  :bind (("s-i" . copy-inner)
         ("s-o" . copy-inner)
         ("s-I" . change-inner)
         ("s-O" . change-inner)))


(use-package discover-my-major
  :ensure t
  :bind (("C-h m" . discover-my-major)))


(use-package docker
  :init
  (defun vlt/docker-compose-up-all ()
    (interactive)
    (docker-compose-run-action-for-all-services "up" nil))
  (defun vlt/docker-compose-down-all ()
    (interactive)
    (docker-compose-run-action-for-all-services "down" nil))
  :ensure t
  :bind (("C-c C-s-d C-s-d" . docker)
         ("C-c C-s-d C-s-u" . vlt/docker-compose-up-all)
         ("C-c C-s-d C-s-w" . vlt/docker-compose-down-all)))


(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")


(use-package docker-compose-mode :ensure t)


(use-package eldoc :diminish eldoc-mode)


(use-package expand-region
  :ensure t
  :bind (("C-'" . 'er/expand-region))
  :config
  (setq expand-region-fast-keys-enabled nil)
  (setq er--show-expansion-message t))


(use-package flycheck
  :ensure t
  :init
  (defun vlt/adjust-flycheck-automatic-syntax-eagerness ()  ;; Another great trick from Magnars
    "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.5 30.0)))
  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  :hook (flycheck-after-syntax-check . vlt/adjust-flycheck-automatic-syntax-eagerness)
  :config
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq flycheck-check-syntax-automatically '(save
                                              idle-change
                                              mode-enabled)))


(use-package move-text
  :ensure t
  :bind (("<C-S-down>" . move-text-down)
         ("<C-S-up>"   . move-text-up)))


(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-¶" . mc/mark-all-like-this) ;; ;
         ("C-ë" . mc/mark-all-in-region) ;; r
         ("C-ä" . mc/mark-all-dwim)))    ;; q


(use-package pass
  :ensure t
  :custom (password-store-password-length 24)
  :config
  (use-package password-store-otp :ensure t))


(use-package project :ensure t)


(use-package restclient :ensure t)


(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  (use-package wgrep :ensure t))


(use-package savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" vlt-defaults/backups-dir))
  :config
  (savehist-mode t))


(use-package saveplace
  :ensure t
  :config (save-place-mode t))


(use-package sql
  :custom (sql-display-sqli-buffer-function t)
  :init
  (defun vlt/sql-login-hook ()
    "I like my postgres to ECHO..."
    (when (eq sql-product'postgres)
      (let ((proc (get-buffer-process (current-buffer))))
        (comint-send-string proc "\\set ECHO queries\n"))))
  :hook (sql-login . vlt/sql-login-hook))


(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat)
              ("C-c C-F" . sqlformat-buffer))
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-g")))


(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode t))


(use-package subword :diminish subword-mode)


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter ""))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-delay 0.5))


(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))


(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all))


(use-package zoom-frm
  :ensure t
  :init
  (defun vlt-packages/enable-zoom-one-shot-keybindings ()  ;; Thanks magnars :)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "=") 'zoom-frm-in)
       (define-key map (kbd "-") 'zoom-frm-out)
       (define-key map (kbd "0") 'zoom-frm-unzoom)
       map)
     t))

  (defun vlt-packages/zoom-frame-in ()
    (interactive)
    (zoom-frm-in)
    (vlt-packages/enable-zoom-one-shot-keybindings))

  (defun vlt-packages/zoom-frame-out ()
    (interactive)
    (zoom-frm-out)
    (vlt-packages/enable-zoom-one-shot-keybindings))
  :bind (("C-x ="   . vlt-packages/zoom-frame-in)
         ("C-x -"   . vlt-packages/zoom-frame-out)
         ("C-x C-0" . zoom-frm-unzoom)))


;; Dired
;; -----------------------------------------------------------------------------

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))


;; Magit
;; -----------------------------------------------------------------------------

(use-package magit
  :ensure t
  :init
  (defun vlt-packages/magit-cursor-fix ()
    (goto-char (point-min))
    (when (looking-at "#")
      (forward-line 2)))
  :bind (("C-x m" . magit-status)
         ("C-s-m b" . magit-blame-addition))
  :config
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))
  (add-hook 'git-commit-mode-hook 'vlt-packages/magit-cursor-fix))


;; Programming Modes
;; -----------------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))



;; Coding utilities
;; -----------------------------------------------------------------------------

(use-package s :ensure t)
(use-package f :ensure t)
(use-package dash :ensure t)


;;; Load all libraries in `libs'
;;  ----------------------------------------------------------------------------

(dolist (lib-path (--remove
                   (or (s-ends-with? "/." it)
                       (s-ends-with? "/.." it))
                   (directory-files (expand-file-name "libs" user-emacs-directory) :full)))
  (add-to-list 'load-path lib-path))


;; For some reason, `:load-path' from `use-package' is not able to find
;; libraries in `libs'.

;; `project-x' helps saving the window/buffer configuration between projects
(require 'project-x)
(project-x-mode 1)

;; Please save everytime we jump to a different project
(defun vlt/project-x-save-before-jumping (orig-func &rest args)
  (project-x-window-state-save))
(advice-add 'project-x-window-state-load :before #'vlt/project-x-save-before-jumping)



;; Require every other package in ~/.emacs.d/packages
;; -----------------------------------------------------------------------------

(dolist (pkg-config (thread-last (expand-file-name "packages" user-emacs-directory)
                      (directory-files)
                      (-remove (lambda (file)
                                 (or (string= file ".")
                                     (string= file "..")
                                     (string= file "vlt-packages.el"))))))
  (load pkg-config))



(provide 'vlt-packages)
;;; vlt-package.el ends here
