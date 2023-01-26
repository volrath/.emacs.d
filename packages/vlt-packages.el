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

(use-package s)
(use-package f)
(use-package dash)
(use-package bind-key)
(use-package diminish)

(require 'bind-key)
(require 'diminish)
(require 'vlt-defuns)


(use-package ace-window
  :bind (("M-z" . ace-window)))


(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode))


(use-package all-the-icons-dired
  :custom (all-the-icons-dired-monochrome nil)
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package anzu
  :diminish anzu-mode
  :bind (("M-%" . 'anzu-query-replace)
         ("C-M-%" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-line)))


(use-package browse-kill-ring
  :bind (("C-x C-y" . browse-kill-ring)))


(use-package change-inner
  :bind (("s-i" . copy-inner)
         ("s-o" . copy-inner)
         ("s-I" . change-inner)
         ("s-O" . change-inner)))


(use-package corfu
  :defer nil
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.5)
  (corfu-preview-current nil)    ;; Disable current candidate preview
  :bind (:map corfu-map ("ESC" . corfu-quit))
  :config
  (setq tab-always-indent 'complete
        completion-cycle-threshold nil)

  (load (expand-file-name "straight/build/corfu/extensions/corfu-info.el" user-emacs-directory))  ;; (use-package corfu-info :load-path "straight/build/corfu/extensions") not working for me 🤔

  (use-package kind-icon
    :after corfu
    :custom
    (kind-icon-use-icons t)
    (kind-icon-default-face 'corfu-default)
    (kind-icon-blend-background nil)
    (kind-icon-blend-frac 0.8)
    (svg-lib-icons-dir (expand-file-name "svg-lib/cache/" vlt/var-dir))
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  (global-corfu-mode))


(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title "Spiral out...")
  (dashboard-set-footer nil)
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda (get-buffer-create "*dashboard*"))))


(use-package define-word
  :bind (("M-#" . define-word-at-point)))


(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))


(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("h" . dired-hide-dotfiles-mode)))


(use-package dired-single
  :init
  (defun vlt/dired-single-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
    ;; <add other stuff here>
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory))
  :config
  (if (boundp 'dired-mode-map)
      (vlt/dired-single-init)
    (add-hook 'dired-load-hook 'vlt/dired-single-init)))


(use-package discover-my-major
  :bind (("C-h m" . discover-my-major)))


(use-package docker
  :init
  (defun vlt/docker-compose-up-all ()
    (interactive)
    (docker-compose-run-action-for-all-services "up" nil))
  (defun vlt/docker-compose-down-all ()
    (interactive)
    (docker-compose-run-action-for-all-services "down" nil))
  :bind (("C-c C-s-d C-s-d" . docker)
         ("C-c C-s-d C-s-u" . vlt/docker-compose-up-all)
         ("C-c C-s-d C-s-w" . vlt/docker-compose-down-all)))


(use-package dockerfile-mode
  :mode "Dockerfile\\'")


(use-package docker-compose-mode)


(use-package eglot
  :hook (python-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t))


(use-package eldoc :diminish eldoc-mode)


(use-package expand-region
  :bind (("C-'" . 'er/expand-region))
  :init
  (require 'clojure-mode-expansions)
  (defun vlt/er-add-sqli-mode-expansions ()
    "Adds ER expansions for buffers in sql-interactive-mode"
    (set (make-local-variable 'er/try-expand-list)
         (append er/try-expand-list
                 ;; Turns out I have what i want in clojure expansions.
                 '(er/mark-clj-word))))
  :config
  (er/enable-mode-expansions 'sql-interactive-mode 'vlt/er-add-sqli-mode-expansions))


(use-package explain-pause-mode)


(use-package flycheck
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


(use-package highlight-indent-guides
  :hook (python-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'bitmap))


(use-package ibuffer-project
  :config
  (defun vlt--ibuffer-project.el-hook ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  (add-hook 'ibuffer-hook #'vlt--ibuffer-project.el-hook))


(use-package json-mode)


(use-package langtool
  :config
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))


(use-package magit
  :init
  (defun vlt-packages/magit-cursor-fix ()
    (goto-char (point-min))
    (when (looking-at "#")
      (forward-line 2)))
  :bind (("C-x m" . magit-status)
         ("C-s-m b" . magit-blame-addition))
  :custom
  (transient-history-file (expand-file-name "transient-history.el" vlt/var-dir))
  :config
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))
  (add-hook 'git-commit-mode-hook 'vlt-packages/magit-cursor-fix))


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
  :init
  (defun vlt/markdown-insert-pull-requests (pr-link)
    "Prompts for a pull request link and inserts a nicely formatted link."
    (interactive "sPull Request: ")
    (let ((pr-number (save-match-data
                       (and (string-match "github.com/\[^\/\]+/\[^\/\]+/pull/\\(\[0-9\]+\\)/?"
                                          pr-link)
                            (match-string 1 pr-link)))))
      (if pr-number
          ;; TODO: consider using `markdown-insert-link'
          (insert (format "[#%s](%s)" pr-number pr-link))
        (error (format "Input does not seem to be a valid Github PR Link %s" pr-link)))))
  :bind (:map markdown-mode-map
              ("C-c p" . vlt/markdown-insert-pull-requests)))


(use-package move-text
  :bind (("<C-S-down>" . move-text-down)
         ("<C-S-up>"   . move-text-up)))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-¶" . mc/mark-all-like-this) ;; ;
         ("C-ë" . mc/mark-all-in-region) ;; r
         ("C-ä" . mc/mark-all-dwim)))    ;; q


(use-package notmuch
  :custom
  (notmuch-show-logo nil)  ;; TODO: consider using your own logo (`notmuch-hello-logo')
  (notmuch-search-oldest-first nil)
  :bind (("H-m" . notmuch)))


(use-package org
  :after smartparens
  :custom
  (org-src-fontify-natively t)
  (org-export-backends '(ascii html icalendar latex md odt))
  (org-default-notes-file (expand-file-name "agenda.org" org-directory))
  (org-agenda-files (list (expand-file-name "agenda.org" org-directory)))
  (org-return-follows-link t)
  (org-enforce-todo-dependencies t)
  (org-persist-directory (expand-file-name "org-persist/" vlt/var-dir))
  :bind (("C-s-o a" . org-agenda)
         ("C-s-o x" . org-capture)
         ("C-s-o l" . org-store-link))
  :config
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

  (use-package org-journal
    :custom
    (org-journal-dir "~/org/journal/"))

  (define-obsolete-function-alias 'org-define-error 'define-error "28.1"))


(use-package page-break-lines
  :diminish page-break-lines-mode
  :custom (page-break-lines-max-width 80)
  :init
  (defun vlt/page-navigation-move-to-bol (&rest _args)
    "page-break-lines is great, but I don't like that the cursor
    goes to the end of the break line when I `backward-page' or
    `forward-page'."
    (when (bound-and-true-p page-break-lines-mode)
      (move-beginning-of-line nil)))
  :config
  (add-to-list 'page-break-lines-modes 'clojure-mode)
  (global-page-break-lines-mode)
  (advice-add 'backward-page :after #'vlt/page-navigation-move-to-bol)
  (advice-add 'forward-page :after #'vlt/page-navigation-move-to-bol))


(use-package pass
  :custom (password-store-password-length 24)
  :bind (("H-p" . pass))
  :config
  (use-package password-store-otp))


(use-package powerthesaurus
  :bind (("C-M-#" . powerthesaurus-lookup-dwim)))


(use-package project
  :config
  (use-package project-x
    :straight (project-x
               :type git
               :host github
               :repo "karthink/project-x"
               :files ("*" (:exclude ".git")))
    :custom
    (project-x-window-list-file (expand-file-name "project-window-list" vlt/var-dir))
    :init
    ;; Switch to the last project used
    (defvar vlt/project-last-used nil "Last perspective used.")
    (defun vlt/project-x-advice-save-last-used (next-project)
      (when (and (project-current)
                 (not (string= (project-root (project-current)) next-project)))
        (setq vlt/project-last-used (project-root (project-current)))))

    (defun vlt/project-x-window-state-load-last-used ()
      (interactive)
      (if vlt/project-last-used
          (project-x-window-state-load vlt/project-last-used)
        (call-interactively 'project-x-window-state-load)))

    ;; Please save everytime we jump to a different project
    (defun vlt/project-x-advice-save-before-jumping (&rest _args)
      (when (project-current)
        (project-x-window-state-save)))

    :bind (:map project-prefix-map
                ("-" . vlt/project-x-window-state-load-last-used))

    :config
    (project-x-mode 1)
    (advice-add 'project-x-window-state-load :before #'vlt/project-x-advice-save-last-used)
    (advice-add 'project-x-window-state-load :before #'vlt/project-x-advice-save-before-jumping)))


(use-package restclient)


(use-package re-builder)


(use-package rg
  :config
  (rg-enable-default-bindings)
  (use-package wgrep))




(use-package smartparens
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ;; Navigation
        ("M-e"   . sp-end-of-sexp)
        ("M-a"   . sp-beginning-of-sexp)
        ("C-M-a" . sp-backward-up-sexp)
        ("C-M-e" . sp-up-sexp)
        ("M-d"   . sp-down-sexp)
        ("M-f"   . sp-forward-sexp)
        ("M-b"   . sp-backward-sexp)
        ("M-n"   . sp-next-sexp)
        ("M-p"   . sp-previous-sexp)

        ;; Editing
        ("M-k"     . sp-kill-sexp)
        ("M-r"     . sp-raise-sexp)
        ("C-)"     . sp-forward-slurp-sexp)
        ("C-("     . sp-forward-barf-sexp)
        ("C-M-("   . sp-backward-slurp-sexp)
        ("C-M-)"   . sp-backward-barf-sexp)
        ("C-x C-t" . sp-transpose-hybrid-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward))
  :defer nil
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode)

  (define-key smartparens-mode-map (kbd "M-(") (vlt/wrap-with "("))
  (define-key smartparens-mode-map (kbd "M-\"") (vlt/wrap-with "\""))

  (use-package hydra
    :config
    (define-key smartparens-mode-map (kbd "C-M-s")
      (defhydra smartparens-hydra ()
        "Smartparens"
        ("d" sp-down-sexp "Down")
        ("e" sp-up-sexp "Up")
        ("u" sp-backward-up-sexp "Up")
        ("a" sp-backward-down-sexp "Down")
        ("f" sp-forward-sexp "Forward")
        ("b" sp-backward-sexp "Backward")
        ("k" sp-kill-sexp "Kill" :color blue)
        ("q" nil "Quit" :color blue)))))


(use-package sql
  :custom (sql-display-sqli-buffer-function t)
  :init
  (defun vlt/sql-login-hook ()
    "I like my postgres to ECHO..."
    (when (eq sql-product 'postgres)
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
  :config (smooth-scrolling-mode t))


(use-package subword :diminish subword-mode)


(use-package sudo-edit)


(use-package swiper
  :init
  (defun vlt-swiper-at-point (sym)
    "Use `swiper' to search for the SYM at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("M-s /" . vlt-swiper-at-point)))


(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))


(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(use-package tree-sitter-langs :after tree-sitter)


(use-package typescript-mode
  :after (eglot tree-sitter)
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure
  ;; out language for server see https://github.com/joaotavora/eglot/issues/624
  ;; and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  (add-to-list 'eglot-server-programs '(typescriptreact-mode . ("typescript-language-server" "--stdio")))

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode ->
  ;; treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))


(use-package undo-tree
  :diminish undo-tree-mode
  :custom (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" vlt/var-dir))))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter ""))


(use-package unfill)


(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-delay 0.5))


(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))


(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))


(use-package zoom-frm
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
