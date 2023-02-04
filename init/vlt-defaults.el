;;; vlt-defaults.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-defaults.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sat Jan 29 17:37:47 2022 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is a mix of Phil Hagelberg's `better-defaults.el', prelude's
;; `prelude-editor.el', magnar's `sane-defaults.el' and some defaults
;; of my own.
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

(require 'ansi-color)
(require 'auth-source)
(require 'compile)
(require 'misc)
(require 'tramp)


;; auth-source in `pass'
(setq auth-sources '(password-store))

;; Window good behavior
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Buffers good behavior
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; History
(require 'saveplace)  ;; Save point position between sessions
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" vlt/var-dir))

(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" vlt/var-dir))
(savehist-mode t)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" vlt/var-dir)
      recentf-max-saved-items 100
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode t)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Auto refresh buffers
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)  ; Also auto refresh dired
(setq auto-revert-verbose nil)                ; but be quiet about it

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; prettify symbols
(global-prettify-symbols-mode t)

;; Show me wrapping parens
(show-paren-mode t)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; the blinking cursor is nothing but an annoyance
(blink-cursor-mode -1)

;; Show active region
(transient-mark-mode t)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; So long, long lines
(global-so-long-mode 1)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(defun vlt-defaults/colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))
(add-hook 'compilation-filter-hook #'vlt-defaults/colorize-compilation-buffer)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  "Offer to create parent directories if they do not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)


;; Only autofill in comments!
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-auto-fill-only-comments) t)))


;; Highlight comment annotations
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
                    1 font-lock-warning-face t)))))

;; Python needs some defaults
(add-hook 'python-mode-hook (lambda ()
                              (electric-indent-mode t)
                              (display-fill-column-indicator-mode t)))


;; Tramp settings
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))


;; Email
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587)  ;; server & user in private.el


;; macOS Settings (🤯🔫)
(when (eq system-type 'darwin)
  (setq
   ;; Remap Modifiers
   ;; I use Karabiner's simple rules to get the following layout:
   ;; +------+------+------+------+-----------+
   ;; | Cmd  | Fn   | Optn | Ctrl | Space Bar | ... (I don't really care about right mods)
   ;; +------+------+------+------+-----------+
   mac-command-modifier  'control
   mac-control-modifier  'meta
   mac-option-modifier   'super
   mac-function-modifier 'hyper
   ;; Random sanity
   delete-by-moving-to-trash t))


;; Sanity...
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-message t
      ;; font-lock-maximum-decoration t
      ;; color-theme-is-global t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      display-time-default-load-average nil  ; I can live without the load-average, thank you.
      dired-kill-when-opening-new-dired-buffer t
      ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-window-setup-function 'ediff-setup-windows-plain
      gc-cons-threshold (* 100 1024 1024)  ; Don't be so stingy on the memory, we have lots now. It's the distant future.
      read-process-output-max (* 1024 1024)
      large-file-warning-threshold 100000000  ; 100MB might be too much
      save-place-file (expand-file-name "saveplace" vlt/var-dir)  ; Save place in buffers
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" vlt/var-dir)
      backup-directory-alist `(("." . ,vlt/var-dir))  ; Keep things clean!
      bookmark-default-file (expand-file-name "bookmarks" vlt/var-dir)
      truncate-partial-width-windows nil  ; Don't truncate lines
      redisplay-dont-pause t  ; Don't defer screen updates when performing operations
      echo-keystrokes 0.1  ; Show keystrokes in progress
      delete-by-moving-to-trash t  ; Move files to trash when deleting
      shift-select-mode nil  ; Real emacs knights don't use shift to mark things
      x-select-enable-clipboard t  ; Allow pasting selection outside of Emacs
      enable-recursive-minibuffers t  ; Allow recursive minibuffers
      xterm-mouse-mode t  ; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text_002dOnly-Mouse.html
      ispell-program-name "aspell"
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first error
      electric-indent-mode nil
      org-replace-disputed-keys t ; Don't let org-mode ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
      org-src-fontify-natively t ; Fontify org-mode code blocks
      tramp-persistency-file-name (expand-file-name "tramp" vlt/var-dir)
      nsm-settings-file (expand-file-name "network-security.data" vlt/var-dir)
      )
(load custom-file)

(provide 'vlt-defaults)

;;; vlt-defaults.el ends here
